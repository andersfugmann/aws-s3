open StdLabels
let sprintf = Printf.sprintf

type meth = [ `DELETE | `GET | `HEAD | `POST | `PUT ]

module Make(Io : Types.Io) = struct
  module Body = Body.Make(Io)
  open Io
  open Deferred

  let string_of_method = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `DELETE -> "DELETE"

  let read_status reader remain =
    (* Start reading the reply *)
    Body.read_until ~sep:" " reader remain >>=? fun (_http_version, remain) ->
    Body.read_until ~sep:" " reader remain >>=? fun (status_code, remain) ->
    Body.read_until ~sep:"\r\n" reader remain >>=? fun (status_message, remain) ->
    Or_error.return ((int_of_string status_code, status_message), remain)

  let read_headers reader remain =
    let rec inner acc remain =
      Body.read_until ~sep:"\r\n" reader remain >>=? function
      | ("", remain) -> Or_error.return (acc, remain)
      | (line, remain) ->
        let (key, value) =
          match Str.split (Str.regexp ": ") line with
          | [] -> failwith "Illegal header"
          | [ k ] -> (k, "")
          | [ k; v ] -> (k, v)
          | k :: vs -> (k, String.concat ~sep:": " vs)
        in
        inner (Headers.add ~key ~value acc) remain
    in
    inner Headers.empty remain


  let call ?(domain=Unix.PF_INET) ?(expect=false) ~scheme ~host ~path ?(query=[]) ~headers ?body (meth:meth) =
    let headers = match expect with
      | true -> Headers.add ~key:"Expect" ~value:"100-continue" headers
      | false -> headers
    in
    Net.connect ~domain ~host ~scheme >>=? fun (reader, writer) ->
    let path_with_params = match query with
      | [] -> path
      | query ->
        let query =
          List.map ~f:(fun (k, v) -> sprintf "%s=%s" k v) query
          |> String.concat ~sep:"&"
        in
        sprintf "%s?%s" path query
    in
    let header = sprintf "%s %s HTTP/1.1\r\n" (string_of_method meth) path_with_params in
    Pipe.write writer header >>= fun () ->
    (* Write all headers *)
    Headers.fold (fun key value acc ->
        acc >>= fun () ->
        Pipe.write writer key >>= fun () ->
        Pipe.write writer ": " >>= fun () ->
        Pipe.write writer value >>= fun () ->
        Pipe.write writer "\r\n" >>= fun () ->
        return ()
      ) headers (return ()) >>= fun () ->
    Pipe.write writer "\r\n" >>= fun () ->

    begin
      match expect with
      | true ->
        read_status reader "" >>=? fun ((status_code, status_message), remain) ->
        begin match status_code with
        | 100 ->
          begin match body with
          | None -> return ()
          | Some body -> Pipe.transfer body writer
          end >>= fun () ->
          read_status reader remain
        | _ -> Or_error.return ((status_code, status_message), remain)
        end
      | false ->
        begin match body with
        | None -> return ()
        | Some body -> Pipe.transfer body writer
        end >>= fun () ->
        read_status reader ""
    end >>=? fun ((status_code, status_message), remain) ->
    Pipe.close writer;

    read_headers reader remain >>=? fun (headers, remain) ->

    (* Test if the reply is chunked *)
    let chunked_transfer =
      match Headers.find_opt "transfer-encoding" headers with
      | Some encoding ->
        List.mem "chunked" ~set:(String.split_on_char ~sep:',' encoding)
      | None -> false
    in
    let body = match (meth, Headers.find_opt "content-length" headers, chunked_transfer) with
      | (`HEAD, _, _)
      | (_, None, false)
      | (_, Some "0", false) ->
        Pipe.close writer;
        Pipe.close_reader reader;
        Pipe.create_reader ~f:(fun _ -> return ())
      | _, Some length, false ->
        let length = int_of_string length in
        let body = Body.of_pipe ~length ~start:remain reader in
        async (Pipe.closed body >>= fun () -> Pipe.close writer; Pipe.close_reader reader; return ());
        body
      | _, None, true ->
        let body = Body.chunk_reader reader remain in
        async (Pipe.closed body >>= fun () -> Pipe.close writer; Pipe.close_reader reader; return ());
        body
      | _, Some _length, true ->
        failwith "Chunked transfer contained content length"
    in
    Or_error.return (status_code, status_message, headers, body)
end
