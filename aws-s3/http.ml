(**/**)
open StdLabels
let sprintf = Printf.sprintf
let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt

type meth = [ `DELETE | `GET | `HEAD | `POST | `PUT ]

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

  let send_request ~expect ~path ~query ~headers ~meth writer () =
    let headers = match expect with
      | true -> Headers.add ~key:"Expect" ~value:"100-continue" headers
      | false -> headers
    in
    let path_with_params =
      let query = List.map ~f:(fun (k, v) -> k, [v]) query in
      Uri.make ~path ~query () |> Uri.to_string
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
    return ()

  let handle_expect ~expect reader =
    match expect with
    | true -> begin
      log "Expect 100-continue";
      read_status reader "" >>=? function
      | ((100, _), remain) ->
        log "Got 100-continue";
        Or_error.return (`Continue remain)
      | ((code, message), remain) ->
        Or_error.return (`Failed ((code, message), remain))
      end
    | false -> Or_error.return (`Continue "")

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

  let read_data ~sink ~headers remain reader =
    (* Test if the reply is chunked *)
    let chunked_transfer =
      match Headers.find_opt "transfer-encoding" headers with
      | Some encoding ->
        List.mem "chunked" ~set:(String.split_on_char ~sep:',' encoding)
      | None -> false
    in
    begin
      match (Headers.find_opt "content-length" headers, chunked_transfer) with
      | (None, false)
      | (Some "0", false) ->
        Or_error.return "" (* This is just an optimization *)
      | Some length, false ->
        let length = int_of_string length in
        Body.transfer ~length ~start:remain reader sink
      | _, true -> (* Actually we should not accept a content
                      length then when encoding is chunked, but AWS
                      does require this when uploading, so we
                      accept it for symmetry.*)
        Body.chunked_transfer ~start:remain reader sink
    end >>=? fun _remain ->
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
        read_status reader remain
    end >>=? fun ((code, message), remain) ->
    read_headers reader remain >>=? fun (headers, remain) ->

    begin match meth with
      | `HEAD -> Or_error.return ""
      | _ ->
        let sink, error_body = match code with
          | n when 200 <= n && n < 300 -> (sink, None)
          | _ ->
            (* Capture the response as an error message *)
            let body, writer = Body.reader () in
            (* Close the sink. *)
            Pipe.close sink;
            writer, Some body
        in
        read_data ~sink ~headers remain reader >>=? fun () ->
        begin
          match error_body with
          | None -> Or_error.return ""
          | Some body -> Body.get body
        end
    end >>=? fun error_body ->
    Or_error.return (code, message, headers, error_body)


  let call ?(domain=Unix.PF_INET) ?(expect=false) ~scheme ~host ~path ?(query=[]) ~headers ~sink ?body (meth:meth) =
    Net.connect ~domain ~host ~scheme >>=? fun (reader, writer) ->
    (* At this point we need to make sure reader and writer are closed properly. *)
    do_request ~expect ~path ~query ~headers ~sink ?body meth reader writer >>= fun result ->
    (* Close the reader and writer regardless of status *)
    Pipe.close_reader reader;
    Pipe.close writer;
    return result
end
