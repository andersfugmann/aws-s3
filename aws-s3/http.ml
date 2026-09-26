(**/**)
open StdLabels
let sprintf = Printf.sprintf
let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt

type meth = [ `DELETE | `GET | `HEAD | `POST | `PUT ]

(* The request target must reach the wire exactly as it was signed, so encode it
   here rather than routing it through [Uri], which decodes reserved characters
   ("%2B" back to "+") and breaks the signature. *)
let request_target ~path ~query =
  let encoded_query =
    query
    |> List.map ~f:(fun (k, v) ->
           sprintf "%s=%s"
             (Uri.pct_encode ~component:`Userinfo k)
             (Uri.pct_encode ~component:`Userinfo v))
    |> String.concat ~sep:"&"
  in
  let path = Util.encode_string path in
  if encoded_query = "" then path else sprintf "%s?%s" path encoded_query

let%test "request_target keeps '+' percent-encoded" =
  request_target ~path:"/a+b" ~query:[] = "/a%2Bb"

let%test "request_target path matches the signed encoding" =
  let path = "/tsync/Audiosocket Info Sheet + Tax Docs.pdf" in
  request_target ~path ~query:[] = Util.encode_string path

let%test "request_target appends the encoded query" =
  request_target ~path:"/k" ~query:[("prefix", "a+b")] = "/k?prefix=a%2Bb"

module Make(Io : Types.Io) = struct
  module Body = Body.Make(Io)
  open Io
  open Deferred

  let string_of_method = function
  | `GET    -> "GET"
  | `PUT    -> "PUT"
  | `HEAD   -> "HEAD"
  | `POST   -> "POST"
  | `DELETE -> "DELETE"

  let read_status ?start reader =
    let remain = start in
    (* Start reading the reply *)
    Body.read_until ?start:remain ~sep:" " reader >>=? fun (_http_version, remain) ->
    Body.read_until ?start:remain ~sep:" " reader >>=? fun (status_code, remain) ->
    Body.read_until ?start:remain ~sep:"\r\n" reader >>=? fun (status_message, remain) ->
    Or_error.return ((int_of_string status_code, status_message), remain)

  let read_headers ?start reader =
    let rec inner ?start acc =
      Body.read_until ?start ~sep:"\r\n" reader >>=? function
      | ("", remain) -> Or_error.return (acc, remain)
      | (line, remain) ->
        let (key, value) =
          match Str.split (Str.regexp ": ") line with
          | [] -> failwith "Illegal header"
          | [ k ] -> (k, "")
          | [ k; v ] -> (k, v)
          | k :: vs -> (k, String.concat ~sep:": " vs)
        in
        inner ?start:remain (Headers.add ~key ~value acc)
    in
    inner ?start Headers.empty

  let send_request ~expect ~path ~query ~headers ~meth writer () =
    let headers = match expect with
      | true -> Headers.add ~key:"Expect" ~value:"100-continue" headers
      | false -> headers
    in
    let path_with_params = request_target ~path ~query in
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
    return ()

  let handle_expect ~expect reader =
    match expect with
    | true -> begin
      log "Expect 100-continue";
      read_status reader >>=? function
      | ((100, _), remain) ->
        log "Got 100-continue";
        Or_error.return (`Continue remain)
      | ((code, message), remain) ->
        Or_error.return (`Failed ((code, message), remain))
      end
    | false -> Or_error.return (`Continue None)

  let send_body ?body writer =
    let rec transfer reader writer =
      Pipe.read reader >>= function
      | Some data ->
        Pipe.write writer data >>= fun () ->
        transfer reader writer
      | None -> return ()
    in
    match body with
    | None -> Or_error.return ()
    | Some reader ->
      catch (fun () -> transfer reader writer) >>= fun result ->
      (* Close the reader and writer in any case *)
      Pipe.close_reader reader;
      return result (* Might contain an exception *)

  let read_data ?start ~sink ~headers reader =
    (* Test if the reply is chunked *)
    let chunked_transfer =
      match Headers.find_opt "transfer-encoding" headers with
      | Some encoding ->
        List.mem "chunked" ~set:(String.split_on_char ~sep:',' encoding)
      | None -> false
    in
    begin
      match (Headers.find_opt "content-length" headers, chunked_transfer) with
      | (None, false) -> Or_error.return None
      | Some length, false ->
        let length = int_of_string length in
        Body.transfer ?start ~length reader sink
      | _, true -> (* Actually we should not accept a content
                      length then when encoding is chunked, but AWS
                      does require this when uploading, so we
                      accept it for symmetry.*)
        Body.chunked_transfer ?start reader sink
    end >>=? fun _remain ->
    (* We could log here is we have extra data *)
    Pipe.close sink;
    Or_error.return ()

  let do_request ~expect ~path ?(query=[]) ~headers ~sink ?body meth reader writer =
    catch (send_request ~expect ~path ~query ~headers ~meth writer) >>=? fun () ->
    begin
      handle_expect ~expect reader >>=? function
      | `Failed ((code, message), remain) ->
        Or_error.return ((code, message), remain)
      | `Continue remain ->
        send_body ?body writer >>=? fun () ->
        read_status ?start:remain reader
    end >>=? fun ((code, message), remain) ->
    read_headers ?start:remain reader >>=? fun (headers, remain) ->


    let error_body, error_sink =
      let reader, writer = Pipe.create () in
      Body.to_string reader, writer
    in

    begin match meth with
      | `HEAD -> Or_error.return ""
      | _ ->
        let sink = match code with
          | n when 200 <= n && n < 300 ->
            Pipe.close error_sink;
            sink
          | _ ->
            Pipe.close sink;
            error_sink
        in
        read_data ?start:remain ~sink ~headers reader >>=? fun () ->
        error_body >>= fun error_body ->
        Or_error.return error_body
    end >>=? fun error_body ->
    Or_error.return (code, message, headers, error_body)


  let call ?(expect=false) ?connect_timeout_ms ~(endpoint:Region.endpoint) ~path ?(query=[]) ~headers ~sink ?body (meth:meth) =
    Net.connect ?connect_timeout_ms ~inet:endpoint.inet ~host:endpoint.host ~port:endpoint.port ~scheme:endpoint.scheme () >>=? fun (reader, writer) ->
    (* At this point we need to make sure reader and writer are closed properly. *)
    do_request ~expect ~path ~query ~headers ~sink ?body meth reader writer >>= fun result ->
    (* Close the reader and writer regardless of status *)
    Pipe.close writer;
    Pipe.close_reader reader;
    Pipe.close sink;
    return result
end
