(**/**)
open StdLabels
let sprintf = Printf.sprintf
let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt

type meth = Types.meth

let string_of_method = function
  | `GET    -> "GET"
  | `PUT    -> "PUT"
  | `HEAD   -> "HEAD"
  | `POST   -> "POST"
  | `DELETE -> "DELETE"

(* [Transfer-Encoding] as a list of lowercase tokens. *)
let transfer_codings headers =
  match Headers.find_opt "transfer-encoding" headers with
  | None -> []
  | Some encoding ->
    String.split_on_char ~sep:',' encoding
    |> List.map ~f:(fun coding -> String.trim coding |> String.lowercase_ascii)

let wants_close headers =
  match Headers.find_opt "connection" headers with
  | None -> false
  | Some v ->
    String.split_on_char ~sep:',' v
    |> List.exists ~f:(fun token -> String.trim token |> String.lowercase_ascii = "close")

(* Reuse requires that the whole response body was consumed, which only holds
   when [read_data] knew where the body ended; otherwise leftover bytes corrupt
   the next request. A transfer-encoding other than a lone [chunked] is not
   decoded here, so it is not framed for our purposes. *)
let keeps_alive ~meth ~code ~req_headers ~headers =
  let framed =
    meth = `HEAD
    || code = 204 || code = 304
    || Headers.find_opt "content-length" headers <> None
    || transfer_codings headers = ["chunked"]
  in
  framed && not (wants_close req_headers) && not (wants_close headers)

(* A retry replays the request, so it is only safe for a method S3 defines as
   idempotent: [Multipart.initiate] is a bodyless POST, and replaying it creates
   a second upload. *)
let idempotent = function
  | `GET | `HEAD | `PUT | `DELETE -> true
  | `POST -> false

let pool_key (endpoint : Region.endpoint) =
  let scheme = match endpoint.scheme with `Http -> "http" | `Https -> "https" in
  sprintf "%s:%s:%d" scheme endpoint.host endpoint.port

let%test "keeps_alive" =
  let headers l =
    List.fold_left ~init:Headers.empty ~f:(fun acc (key, value) -> Headers.add ~key ~value acc) l
  in
  let alive ?(meth=`GET) ?(code=200) ?(req=[]) l =
    keeps_alive ~meth ~code ~req_headers:(headers req) ~headers:(headers l)
  in
  alive ["content-length", "3"]
  && alive ["transfer-encoding", "chunked"]
  && alive ["transfer-encoding", "Chunked"]
  && alive ~meth:`HEAD []
  && alive ~meth:`DELETE ~code:204 []
  (* Unframed, so the next response would start at an unknown offset. *)
  && not (alive [])
  && not (alive ["transfer-encoding", "gzip"])
  && not (alive ["transfer-encoding", "gzip, chunked"])
  && not (alive ["content-length", "3"; "connection", "close"])
  && not (alive ~req:["connection", "Close"] ["content-length", "3"])

module type Pool_config = sig
  val max_idle_per_host : int
  val max_idle_total : int
  val max_idle_age : float
end

module Default_pool_config = struct
  let max_idle_per_host = 32
  let max_idle_total = 64
  let max_idle_age = 20.
end

module Make_pooled(Io : Types.Io)(Config : Pool_config) = struct
  module Io = Io
  module Body = Body.Make(Io)
  open Io
  open Deferred

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
    let chunked_transfer = List.mem "chunked" ~set:(transfer_codings headers) in
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

  let do_request ~expect ~path ?(query=[]) ~headers ~sink ~sink_used ?body meth reader writer =
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
    sink_used := true;

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


  (* Idle connections to the same (scheme, host, port) are kept open and reused,
     saving a TCP + TLS handshake per request. Each entry carries the time it
     was returned to the pool. *)
  let pool : (string, ((string Pipe.reader * string Pipe.writer) * float) Queue.t) Hashtbl.t =
    Hashtbl.create 8

  (* Closing the reader cascades to the socket close, see [Net.connect]. *)
  let discard_conn (reader, writer) =
    Pipe.close writer;
    Pipe.close_reader reader

  (* An endpoint visited once must not leave an entry behind for the lifetime of
     the process, so the queue is dropped as soon as it runs empty. *)
  let rec take_idle key =
    match Hashtbl.find_opt pool key with
    | None -> None
    | Some queue ->
      match Queue.take_opt queue with
      | None -> Hashtbl.remove pool key; None
      | Some (((reader, writer) as conn), idle_since) ->
        let stale =
          Pipe.is_closed reader || Pipe.is_closed writer
          || Unix.gettimeofday () -. idle_since > Config.max_idle_age
        in
        match stale with
        | true -> discard_conn conn; take_idle key
        | false -> Some conn

  let idle_total () =
    Hashtbl.fold (fun _ queue total -> total + Queue.length queue) pool 0

  (* Entries are appended, so the front of each queue is its oldest and the
     global cap can free room without walking whole queues. *)
  let evict_oldest () =
    let oldest =
      Hashtbl.fold (fun key queue acc ->
          match Queue.peek_opt queue, acc with
          | None, _ -> acc
          | Some (_, idle_since), Some (_, previous) when previous <= idle_since -> acc
          | Some (_, idle_since), _ -> Some (key, idle_since))
        pool None
    in
    match oldest with
    | None -> ()
    | Some (key, _) ->
      match Hashtbl.find_opt pool key with
      | None -> ()
      | Some queue ->
        (match Queue.take_opt queue with
         | Some (conn, _) -> discard_conn conn
         | None -> ());
        match Queue.is_empty queue with
        | true -> Hashtbl.remove pool key
        | false -> ()

  let return_idle key conn =
    let queue =
      match Hashtbl.find_opt pool key with
      | Some queue -> queue
      | None -> Queue.create ()
    in
    (* One insertion can exceed the total by at most one, so one eviction is
       enough to make room unless the pool is capped at nothing. *)
    let room =
      Queue.length queue < Config.max_idle_per_host
      && (idle_total () < Config.max_idle_total
          || (evict_oldest (); idle_total () < Config.max_idle_total))
    in
    match room with
    | false -> discard_conn conn
    | true ->
      Queue.add (conn, Unix.gettimeofday ()) queue;
      Hashtbl.replace pool key queue

  let get_connection ?connect_timeout_ms (endpoint : Region.endpoint) =
    match take_idle (pool_key endpoint) with
    | Some conn -> Deferred.Or_error.return (`Reused conn)
    | None ->
      Net.connect ?connect_timeout_ms ~inet:endpoint.inet ~host:endpoint.host
        ~port:endpoint.port ~scheme:endpoint.scheme ()
      >>=? fun conn -> Deferred.Or_error.return (`Fresh conn)

  let call ?(expect=false) ?connect_timeout_ms ~(endpoint:Region.endpoint) ~path ?(query=[]) ~headers ~sink ?body (meth:meth) =
    let key = pool_key endpoint in
    (* Set once the response head is in, which is the point from which
       [do_request] starts writing to [sink] or closing it. *)
    let sink_used = ref false in
    let run (reader, writer) =
      do_request ~expect ~path ~query ~headers ~sink ~sink_used ?body meth reader writer
    in
    let finish conn result =
      (match result with
       | Ok (code, _, resp_headers, _)
         when keeps_alive ~meth ~code ~req_headers:headers ~headers:resp_headers ->
         return_idle key conn
       | _ -> discard_conn conn);
      Pipe.close sink;
      return result
    in
    get_connection ?connect_timeout_ms endpoint >>=? fun tagged ->
    let conn = match tagged with `Fresh c | `Reused c -> c in
    let reused = match tagged with `Reused _ -> true | `Fresh _ -> false in
    run conn >>= fun result ->
    (* A pooled socket may have been dropped by the peer while idle, so a
       failure on a reused connection is retried once on a fresh one -- only
       without a body, which the first attempt consumed and this layer cannot
       replay (PUT relies on the caller's retry, which can rebuild it), and only
       while the caller's sink is still untouched. *)
    match result with
    | Error _ when reused && body = None && idempotent meth && not !sink_used ->
      discard_conn conn;
      Net.connect ?connect_timeout_ms ~inet:endpoint.inet ~host:endpoint.host
        ~port:endpoint.port ~scheme:endpoint.scheme () >>= (function
        | Ok conn -> run conn >>= fun result -> finish conn result
        | Error _ as err -> Pipe.close sink; return err)
    | _ -> finish conn result
end

module Make(Io : Types.Io) = Make_pooled(Io)(Default_pool_config)
