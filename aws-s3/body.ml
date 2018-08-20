open StdLabels

module Make(Io : Types.Io) = struct
  open Io
  open Deferred

  type t =
    | String of string
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

  let read_until ~msg ?start ~sep reader =
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
            Or_error.fail (Failure (Printf.sprintf "%s: EOF while looking for '%d'. Data is: '%S'" msg (Char.code sep.[sep_index]) (Buffer.contents buffer)))
        end
      | sep_index when Buffer.nth buffer offset = sep.[sep_index] ->
        loop (offset + 1) (sep_index + 1)
      | _ ->
        (* Reset sep index. Look for the next element. *)
        loop (offset + 1) 0
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
      read_until ~msg:"read chunk" ?start:remain ~sep:"\r\n" reader >>=? fun (size_str, data) ->
      begin
        try Scanf.sscanf size_str "%x" (fun x -> x) |> Or_error.return
        with _ -> Or_error.fail (Failure "Malformed chunk: Invalid length")
      end >>=? fun chunk_size ->
      match chunk_size with
      | 0 -> read_until ~msg:"end chunk" ?start:data ~sep:"\r\n" reader >>=? fun (_, remain) ->
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
