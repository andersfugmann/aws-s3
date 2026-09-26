open StdLabels

(* Bytes gathered off the OCaml heap. It doubles because a sink is never told
   the content length it is about to receive. *)
module Accumulator = struct
  type t = { mutable buffer: Bigstringaf.t; mutable filled: int }

  let initial_size = 65536

  let create () = { buffer = Bigstringaf.empty; filled = 0 }

  let reserve t needed =
    match Bigstringaf.length t.buffer >= needed with
    | true -> ()
    | false ->
      let rec double size = match size >= needed with
        | true -> size
        | false -> double (size * 2)
      in
      let size = max (Bigstringaf.length t.buffer) initial_size in
      let grown = Bigstringaf.create (double size) in
      Bigstringaf.blit t.buffer ~src_off:0 grown ~dst_off:0 ~len:t.filled;
      t.buffer <- grown

  let add_string t data =
    let len = String.length data in
    reserve t (t.filled + len);
    Bigstringaf.blit_from_string data ~src_off:0 t.buffer ~dst_off:t.filled ~len;
    t.filled <- t.filled + len

  (* A [sub] would share the slack doubling left behind, and the caller holds
     the result for as long as it holds the body. *)
  let contents t =
    match t.filled = Bigstringaf.length t.buffer with
    | true -> t.buffer
    | false -> Bigstringaf.copy t.buffer ~off:0 ~len:t.filled
end

let%test "accumulator answers exactly what was added, in order" =
  let acc = Accumulator.create () in
  let expected = Buffer.create 0 in
  (* Pieces of varying length carrying their own index, so a lost, doubled or
     misplaced one shows up rather than being covered by its neighbour. *)
  for i = 1 to 5000 do
    let piece = Printf.sprintf "%d:%s|" i (String.make (i mod 97) 'x') in
    Buffer.add_string expected piece;
    Accumulator.add_string acc piece
  done;
  let result = Accumulator.contents acc in
  let expected = Buffer.contents expected in
  String.length expected > Accumulator.initial_size
  && Bigstringaf.length result = String.length expected
  && Bigstringaf.to_string result = expected

let%test "an accumulator nothing was added to is empty" =
  Bigstringaf.length (Accumulator.contents (Accumulator.create ())) = 0

module Make(Io : Types.Io) = struct
  open Io
  open Deferred

  type t =
    | String of string
    | Bigstring of Bigstringaf.t
    | Empty
    | Chunked of { pipe: string Pipe.reader; length: int; chunk_size: int }

  let null () =
    let rec read reader =
      Pipe.read reader >>= function
      | None -> return ()
      | Some _ -> read reader
    in
    Pipe.create_writer ~f:read

  let to_string body =
    let rec loop acc =
      Pipe.read body >>= function
      | Some data ->
        loop (data :: acc)
      | None ->
        String.concat ~sep:"" (List.rev acc) |> return
    in
    loop []

  (* Each fragment is dropped as soon as it is gathered, where {!to_string}
     holds every one of them until the end. *)
  let to_bigstring body =
    let acc = Accumulator.create () in
    let rec loop () =
      Pipe.read body >>= function
      | Some data ->
        Accumulator.add_string acc data;
        loop ()
      | None -> return (Accumulator.contents acc)
    in
    loop ()

  let read_string ?start ~length reader =
    let rec loop acc data remain =
      match data, remain with
      | data, 0 -> Or_error.return (Buffer.contents acc, data)
      | None, remain -> begin
        Pipe.read reader >>= function
        | None -> Or_error.fail (Failure "EOF")
        | data -> loop acc data remain
      end
      | Some data, remain when String.length data < remain ->
        Buffer.add_string acc data;
        loop acc None (remain - String.length data)
      | Some data, remain ->
        Buffer.add_substring acc data 0 remain;
        Or_error.return
          (Buffer.contents acc, Some (String.sub data ~pos:remain ~len:(String.length data - remain)))
    in
    loop (Buffer.create length) start length

  let transfer ?start ~length reader writer =
    let rec loop writer data remain =
      match remain, data with
      | 0, data ->
        Or_error.return data
      | remain, Some data -> begin
          match remain - String.length data  with
          | n when n >= 0 ->
              Pipe.write writer data >>= fun () ->
              loop writer None n
          | _ -> (* Only write whats expected and discard the rest *)
            Pipe.write writer (String.sub ~pos:0 ~len:remain data) >>= fun () ->
            loop writer None 0
        end
      | remain, None ->
        begin
          Pipe.read reader >>= function
          | None -> Or_error.fail (Failure "Premature end of input");
          | data -> loop writer data remain
        end
    in
    loop writer start length

  let read_until ?start ~sep reader =
    let buffer =
      let b = Buffer.create 256 in
      match start with
      | Some data -> Buffer.add_string b data; b
      | None -> b
    in
    let rec loop offset = function
      | sep_index when sep_index = String.length sep ->
        (* Found it. Return data *)
        let v = Buffer.sub buffer 0 (offset - String.length sep) in
        let remain =
          match offset < Buffer.length buffer with
          | true -> Some (Buffer.sub buffer offset (Buffer.length buffer - offset))
          | false -> None
        in
        Or_error.return (v, remain)
      | sep_index when offset >= (Buffer.length buffer) -> begin
          Pipe.read reader >>= function
          | Some data ->
            Buffer.add_string buffer data;
            loop offset sep_index;
          | None ->
            Or_error.fail (Failure (Printf.sprintf "EOF while looking for '%d'" (Char.code sep.[sep_index])))
        end
      | sep_index when Buffer.nth buffer offset = sep.[sep_index] ->
        loop (offset + 1) (sep_index + 1)
      | sep_index ->
        (* Reset sep index. Look for the next element. *)
        loop (offset - sep_index + 1) 0
    in
    loop 0 0

  (** Chunked encoding
       format: <len_hex>\r\n<data>\r\n. Always ends with 0 length chunk
    *)
  let chunked_transfer ?start reader writer =
    let rec read_chunk data remain =
      match data, remain with
      | data, 0 -> return (Ok data)
      | Some data, remain when String.length data < remain ->
        Pipe.write writer data >>= fun () ->
        read_chunk None (remain - String.length data)
      | Some data, remain ->
        Pipe.write writer (String.sub ~pos:0 ~len:remain data) >>= fun () ->
        read_chunk (Some (String.sub ~pos:remain ~len:(String.length data - remain) data)) 0
      | None, _ -> begin
          Pipe.read reader >>= function
          | None -> Or_error.fail (Failure "Premature EOF on input")
          | v -> read_chunk v remain
        end
    in
    let rec read remain =
      read_until ?start:remain ~sep:"\r\n" reader >>=? fun (size_str, data) ->
      begin
        try Scanf.sscanf size_str "%x" (fun x -> x) |> Or_error.return
        with _ -> Or_error.fail (Failure "Malformed chunk: Invalid length")
      end >>=? fun chunk_size ->
      match chunk_size with
      | 0 -> read_until ?start:data ~sep:"\r\n" reader >>=? fun (_, remain) ->
        Or_error.return remain
      | n ->
        read_chunk data n >>=? fun data ->
        read_string ?start:data ~length:2 reader >>=? function
        | ("\r\n", data) ->
          read data
        | (_, _data) ->
          Or_error.fail (Failure "Malformed chunk: CRLF not present")
    in
    read start
end
