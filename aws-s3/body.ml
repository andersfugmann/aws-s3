open StdLabels

module Make(Io : Types.Io) = struct
  open Io
  open Deferred
  type t = string Pipe.reader

  type body =
    | String of string
    | Empty
    | Chunked of { pipe: string Pipe.reader; length: int; chunk_size: int }

  type string_body = { writer: string Pipe.writer; content: Buffer.t; }
  let reader ?(size=1024) () =
    let reader, writer = Pipe.create () in
    let content = Buffer.create size in
    let rec read reader =
      Pipe.read reader >>= function
      | None ->
        return ()
      | Some data -> Buffer.add_string content data;
        read reader
    in
    async (read reader);
    let body = { writer; content } in
    body, writer

  let get body =
    match Pipe.is_closed body.writer with
    | true -> Buffer.contents body.content
    | false -> raise (Failure "Body not closed")

  let null () =
    let rec read reader =
      Pipe.read reader >>= function
      | None -> return ()
      | Some _ -> read reader
    in
    Pipe.create_writer ~f:read

  let to_string ?(length = 1024) body =
    let rec loop buffer =
      Pipe.read body >>= function
      | Some data ->
        Buffer.add_string buffer data;
        loop buffer
      | None -> return (Buffer.contents buffer)
    in
    (* Should use the indication from content-length *)
    loop (Buffer.create length)

  let read_string ~length data reader =
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
    loop (Buffer.create length) data length

  let copy ~length ?start reader writer =
    let rec loop writer data remain =
      match remain, data with
      | 0, _ ->
        return ()
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
          | None -> failwith "Not enough data"
          | data -> loop writer data remain
        end
    in
    loop writer start length

  let read_until ~sep reader data =
    let buffer = Buffer.create 256 in
    let rec loop offset = function
      | sep_index when sep_index = String.length sep ->
        (* Found it. Return data *)
        let v = Buffer.sub buffer 0 (offset - String.length sep) in
        let remain = Buffer.sub buffer offset (Buffer.length buffer - offset) in
        return (Ok (v, remain))
      | sep_index when offset >= (Buffer.length buffer) -> begin
          Pipe.read reader >>= function
          | Some data ->
            Buffer.add_string buffer data;
            loop offset sep_index;
          | None ->
            return
              (Error (Failure (Printf.sprintf "EOF while looking for '%d'" (Char.code sep.[sep_index]))))
              (* return (`Eof (Buffer.contents acc)) *)
        end
      | sep_index when Buffer.nth buffer offset = sep.[sep_index] ->
        loop (offset + 1) (sep_index + 1)
      | _ ->
        (* Reset sep index. Look for the next element. *)
        loop (offset + 1) 0
    in
    Buffer.add_string buffer data;
    loop 0 0

  (** Chunked encoding
       format: <len_hex>\r\n<data>\r\n. Always ends with 0 length chunk
    *)
  let chunked_copy ?start reader writer =
    let get_exn = function
      | Error exn -> raise exn
      | Ok v -> return v
    in
    let rec read_chunk data remain =
      match data, remain with
      | data, 0 -> return data
      | Some data, remain when String.length data < remain ->
        Pipe.write writer data >>= fun () ->
        read_chunk None (remain - String.length data)
      | Some data, remain ->
        Pipe.write writer (String.sub ~pos:0 ~len:remain data) >>= fun () ->
        read_chunk (Some (String.sub ~pos:remain ~len:(String.length data - remain) data)) 0
      | None, _ -> begin
          Pipe.read reader >>= function
          | None -> raise (Failure "EOF")
          | v -> read_chunk v remain
        end
    in
    let rec read = function
      | None -> begin
          Pipe.read reader >>= function
          | None -> raise (Failure "EOF")
          | v -> read v
        end
      | Some data -> begin
          read_until ~sep:"\r\n" reader data >>= get_exn >>= fun (size_str, data) ->
          let chunk_size = Scanf.sscanf size_str "%x" (fun x -> x) in
          match chunk_size with
          | 0 -> read_until ~sep:"\r\n" reader data >>= get_exn >>= fun _ ->
            return ()
          | n ->
            read_chunk (Some data) n >>= fun data ->
            read_string ~length:2 data reader >>= get_exn >>= fun (crlf, data) ->
            assert (crlf = "\r\n");
            read data
        end
    in
    read start
end
