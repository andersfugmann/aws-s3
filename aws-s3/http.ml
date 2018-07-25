open StdLabels
let sprintf = Printf.sprintf

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

  let call ~scheme ~host ~path ?(query=[]) ~headers ?body meth =
    Net.connect ~host ~scheme >>=? fun (reader, writer) ->
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
    (* We might want to wait for a result here, if we have expect *)
    (* Transfer all elements from data *)
    begin match body with
    | None -> return ()
    | Some body -> Pipe.transfer body writer (* We wait until all has been transfered *)
    end >>= fun () ->
    Pipe.close writer;

    (* Start reading the reply *)
    Body.read_until ~sep:" " reader "" >>=? fun (_http_version, remain) ->
    Body.read_until ~sep:" " reader remain >>=? fun (status_code, remain) ->
    Body.read_until ~sep:"\r\n" reader remain >>=? fun (status_message, remain) ->

    let rec read_headers (acc: string Headers.t) remain =
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
        read_headers (Headers.add ~key ~value acc) remain
    in
    read_headers Headers.empty remain >>=? fun (headers, remain) ->

    let content_length =
      match Headers.find_opt "content-length" headers with
      | None -> 0
      | Some length ->
        int_of_string length
    in
    (* Test if the reply is chunked *)
    let chunked_transfer =
      match Headers.find_opt "transfer-encoding" headers with
      | Some encoding ->
        List.mem "chunked" ~set:(String.split_on_char ~sep:',' encoding)
      | None -> false
    in
    let body =
      match chunked_transfer with
      | true -> Body.chunk_reader reader remain
      | false -> Body.of_pipe ~length:content_length ~start:remain reader
    in
    (* We need to register a function to close all pipes once the body has been closed. *)
    async (Pipe.closed body >>= fun () -> Pipe.close writer; Pipe.close_reader reader; return ());
    Or_error.return (int_of_string status_code, status_message, headers, body)
end
