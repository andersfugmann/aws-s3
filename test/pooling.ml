(* Counts the connections a canned HTTP/1.1 server accepts, so that reuse,
   a pool configured away, and a peer dropping a reused socket are each
   visible as a connection count. *)
open Lwt.Infix

let response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"

(* Promises ten body bytes, sends two, then drops the connection. *)
let truncated_response = "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nab"

let truncate = ref false

let index_of ~sub string =
  let rec at pos =
    match pos + String.length sub > String.length string with
    | true -> None
    | false ->
      match String.sub string pos (String.length sub) = sub with
      | true -> Some pos
      | false -> at (pos + 1)
  in
  at 0

let write_all socket string =
  let rec loop pos =
    match pos >= String.length string with
    | true -> Lwt.return_unit
    | false ->
      Lwt_unix.write_string socket string pos (String.length string - pos)
      >>= fun written -> loop (pos + written)
  in
  loop 0

(* Requests here carry no body, so a blank line ends one. *)
let serve socket =
  let buffer = Bytes.create 4096 in
  let pending = Buffer.create 256 in
  let rec respond () =
    match index_of ~sub:"\r\n\r\n" (Buffer.contents pending) with
    | None -> Lwt.return_unit
    | Some pos ->
      let rest =
        let contents = Buffer.contents pending in
        String.sub contents (pos + 4) (String.length contents - pos - 4)
      in
      Buffer.clear pending;
      Buffer.add_string pending rest;
      match !truncate with
      | true -> write_all socket truncated_response >>= fun () -> Lwt_unix.close socket
      | false -> write_all socket response >>= respond
  in
  let rec read () =
    Lwt_unix.read socket buffer 0 (Bytes.length buffer) >>= function
    | 0 -> Lwt_unix.close socket
    | count ->
      Buffer.add_subbytes pending buffer 0 count;
      respond () >>= read
  in
  read ()

let start_server accepted =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) >>= fun () ->
  Lwt_unix.listen socket 8;
  let port =
    match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false
  in
  let rec accept_loop () =
    Lwt_unix.accept socket >>= fun (client, _) ->
    incr accepted;
    Lwt.async (fun () -> Lwt.catch (fun () -> serve client) (fun _ -> Lwt.return_unit));
    accept_loop ()
  in
  Lwt.async accept_loop;
  Lwt.return port

module Io = Aws_s3_lwt.Io

module Pooled = Aws_s3.S3.Make_http(Aws_s3.Http.Make(Io))

module Unpooled =
  Aws_s3.S3.Make_http(Aws_s3.Http.Make_pooled(Io)(struct
      let max_idle_per_host = 0
      let max_idle_total = 0
      let max_idle_age = 20.
    end))

(* Its own pool, so the counts here are not confused by what [Pooled] left
   idle. *)
module Retried =
  Aws_s3.S3.Make_http(Aws_s3.Http.Make_pooled(Io)(struct
      let max_idle_per_host = 1
      let max_idle_total = 1
      let max_idle_age = 20.
    end))

let expect ~accepted ~name ~count f =
  accepted := 0;
  let rec repeat n =
    match n with
    | 0 -> Lwt.return_unit
    | n ->
      f () >>= function
      | Ok "ok" -> repeat (n - 1)
      | Ok body -> failwith (Printf.sprintf "%s: unexpected body %S" name body)
      | Error _ -> failwith (Printf.sprintf "%s: request failed" name)
  in
  repeat 3 >>= fun () ->
  match !accepted = count with
  | true -> Lwt_io.printlf "%s: %d connection(s)" name !accepted
  | false ->
    failwith (Printf.sprintf "%s: expected %d connection(s), got %d" name count !accepted)

(* A reused socket the peer drops is retried on a fresh one, but only while
   nothing has reached the caller's sink. *)
let expect_no_retry ~accepted ~get =
  accepted := 0;
  get () >>= fun first ->
  (match first with
   | Ok "ok" -> Lwt.return_unit
   | _ -> failwith "retry: priming request failed") >>= fun () ->
  truncate := true;
  get () >>= fun second ->
  truncate := false;
  match second, !accepted with
  | Ok body, _ -> failwith (Printf.sprintf "retry: expected failure, got %S" body)
  | Error _, 1 -> Lwt_io.printl "truncated: no retry connection"
  | Error _, count ->
    failwith (Printf.sprintf "retry: expected 1 connection(s), got %d" count)

(* [max_idle_total = 1] leaves room for one idle connection in the whole pool,
   so pooling B's evicts A's and A must reconnect. *)
module Capped =
  Aws_s3.S3.Make_http(Aws_s3.Http.Make_pooled(Io)(struct
      let max_idle_per_host = 8
      let max_idle_total = 1
      let max_idle_age = 20.
    end))

let expect_global_cap ~a_accepted ~a_endpoint ~b_endpoint =
  let get endpoint =
    Capped.get ~endpoint ~bucket:"bucket" ~key:"key" () >>= function
    | Ok "ok" -> Lwt.return_unit
    | _ -> failwith "cap: request failed"
  in
  a_accepted := 0;
  get a_endpoint >>= fun () ->
  get b_endpoint >>= fun () ->
  get a_endpoint >>= fun () ->
  match !a_accepted with
  | 2 -> Lwt_io.printl "capped: oldest idle connection evicted"
  | count -> failwith (Printf.sprintf "cap: expected 2 connection(s), got %d" count)

let () =
  Lwt_main.run
    (let accepted = ref 0 and b_accepted = ref 0 in
     start_server accepted >>= fun port ->
     start_server b_accepted >>= fun b_port ->
     let endpoint_at port =
       Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Http
         (Aws_s3.Region.minio ~port ~host:"127.0.0.1" ())
     in
     let endpoint = endpoint_at port in
     expect ~accepted ~name:"pooled" ~count:1 (fun () ->
         Pooled.get ~endpoint ~bucket:"bucket" ~key:"key" ()) >>= fun () ->
     expect ~accepted ~name:"unpooled" ~count:3 (fun () ->
         Unpooled.get ~endpoint ~bucket:"bucket" ~key:"key" ()) >>= fun () ->
     expect_no_retry ~accepted ~get:(fun () ->
         Retried.get ~endpoint ~bucket:"bucket" ~key:"key" ()) >>= fun () ->
     expect_global_cap ~a_accepted:accepted ~a_endpoint:endpoint
       ~b_endpoint:(endpoint_at b_port))
