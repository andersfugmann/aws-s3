(*{{{
 * Copyright (C) 2015 Trevor Smith <trevorsummerssmith@gmail.com>
 * Copyright (C) 2017 Anders Fugmann <anders@fugmann.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Core.Std
open Async.Std
open Cohttp
open Cohttp_async

let ksrt = fun (k,_) (k',_) -> String.compare k k'

module Compat = struct
  (** Things we need to make this happen that, ideally, we'd like other
     libraries to provide and that are orthogonal to the example here *)
  let encode_string s =
    (* Percent encode the path as s3 wants it. Uri doesn't
       encode $, or the other sep characters in a path.
       If upstream allows that we can nix this function *)
    let n = String.length s in
    let buf = Buffer.create (n * 3) in
    for i = 0 to (n-1) do
      let c = String.get s i in
      match c with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_' | '-' | '~' | '.' | '/' -> Buffer.add_char buf c
      | '%' ->
        (* Sigh. Annoying we're expecting already escaped strings so ignore the escapes *)
        begin
          let is_hex = function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
            | _ -> false in
          if (i + 2) < n then
            if is_hex(String.get s (i+1)) && is_hex(String.get s (i+2)) then
              Buffer.add_char buf c
            else
              Buffer.add_bytes buf "%25"
        end
      | _ -> Buffer.add_bytes buf (Printf.sprintf "%%%X" (Char.to_int c))
    done;
    Buffer.contents buf

  let encode_query_string uri =
    (* Sort and encode query string.
       Note that AWS wants null keys to have '=' for all keys.
       Also it seems that amazon is not following standard rules for argument url encoding, but require that
       '+' is pct encoded as '%2B'.
    *)
    Uri.query uri
    |> List.sort ~cmp:ksrt
    |> List.map ~f:(function (k, []) -> (k, [ "" ]) | x -> x)
    |> List.map ~f:(fun (k, vs) ->
        List.map vs ~f:(fun v ->
            sprintf "%s=%s" (Uri.pct_encode ~component:`Userinfo k)
              (Uri.pct_encode ~component:`Userinfo v)
          )
      )
    |> List.concat
    |> String.concat ~sep:"&"

  let format_time t =
    (* Core.Std.Time doesn't have a format function that takes a timezone *)
    let d, s = Time.to_date_ofday ~zone:Time.Zone.utc t in
    let open Core.Span.Parts in
    let {hr; min; sec; _} = Time.Ofday.to_parts s in
    sprintf "%sT%.2d%.2d%.2dZ"
      (Date.to_string_iso8601_basic d) hr min sec

  let hexa = "0123456789abcdef"

  let of_char c =
    let x = Char.to_int c in
    hexa.[x lsr 4], hexa.[x land 0xf]

  let cstruct_to_hex_string cs =
    let open Cstruct in
    let n = cs.len in
    let buf = Buffer.create (n * 2) in
    for i = 0 to n - 1 do
      let c = Bigarray.Array1.get cs.buffer (cs.off+i) in
      let (x,y) = of_char c in
      Buffer.add_char buf x;
      Buffer.add_char buf y;
    done;
    Buffer.contents buf

end

type region = [
  | `Ap_northeast_1 (* Asia Pacific (Tokyo) *)
  | `Ap_southeast_1 (* Asia Pacific (Singapore) *)
  | `Ap_southeast_2 (* Asia Pacific (Sydney) *)
  | `Eu_central_1   (* EU (Frankfurt) *)
  | `Eu_west_1      (* EU (Ireland) *)
  | `Sa_east_1      (* South America (Sao Paulo) *)
  | `Us_east_1      (* US East (N. Virginia) *)
  | `Us_west_1      (* US West (N. California) *)
  | `Us_west_2      (* US West (Oregon) *)
]

let region_of_string = function
  | "ap-northeast-1" -> `Ap_northeast_1
  | "ap-southeast-1" -> `Ap_southeast_1
  | "ap-southeast-2"-> `Ap_southeast_2
  | "eu-central-1" -> `Eu_central_1
  | "eu-west-1" -> `Eu_west_1
  | "sa-east-1" -> `Sa_east_1
  | "us-east-1" -> `Us_east_1
  | "us-west-1" -> `Us_west_1
  | "us-west-2" -> `Us_west_2
  | s -> raise (Invalid_argument ("region_of_string: " ^ s))

let string_of_region = function
  | `Ap_northeast_1 -> "ap-northeast-1"
  | `Ap_southeast_1 -> "ap-southeast-1"
  | `Ap_southeast_2 -> "ap-southeast-2"
  | `Eu_central_1 -> "eu-central-1"
  | `Eu_west_1 -> "eu-west-1"
  | `Sa_east_1 -> "sa-east-1"
  | `Us_east_1 -> "us-east-1"
  | `Us_west_1 -> "us-west-1"
  | `Us_west_2 -> "us-west-2"

let region_host_string = function
  | `Ap_northeast_1 -> "s3-ap-northeast-1.amazonaws.com"
  | `Ap_southeast_1 -> "s3-ap-southeast-1.amazonaws.com"
  | `Ap_southeast_2 -> "s3-ap-southeast-2.amazonaws.com"
  | `Eu_central_1 -> "s3-eu-central-1.amazonaws.com"
  | `Eu_west_1 -> "s3-eu-west-1.amazonaws.com"
  | `Sa_east_1 -> "s3-sa-east-1.amazonaws.com"
  | `Us_east_1 -> "s3.amazonaws.com"
  | `Us_west_1 -> "s3-us-west-1.amazonaws.com"
  | `Us_west_2 -> "s3-us-west-2.amazonaws.com"

module Auth = struct
  (** AWS S3 Authorization *)
  let digest s =
    (* string -> sha256 as a hex string *)
    Nocrypto.Hash.(digest `SHA256 (Cstruct.of_string s))
    |> Compat.cstruct_to_hex_string

  let mac k v = Nocrypto.Hash.(
      mac `SHA256 ~key:k (Cstruct.of_string v))

  let make_amz_headers ?credentials ?body time =
    (* Return x-amz-date and x-amz-sha256 headers *)
    let hashed_payload =
      match body with
        None -> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (* digest "" *)
      | Some s -> digest s
    in
    let token_header = match credentials with
      | Some { Credentials.aws_token = Some token; _ } ->
        [("x-amz-security-token", token)]
      | _ -> []
    in
    let headers = [
      ("x-amz-content-sha256", hashed_payload);
      ("x-amz-date", Compat.format_time time)
    ] @ token_header
    in
    (headers, hashed_payload)

  let canonical_request hashed_payload (request : Cohttp_async.Request.t) =
    (* This corresponds to p.21 of the s3 api doc
       we're making:
       <HTTPMethod>\n
       <CanonicalURI>\n
       <CanonicalQueryString>\n
       <CanonicalHeaders>\n
       <SignedHeaders>\n
       <HashedPayload>
    *)
    let open Cohttp.Request in
    let http_method = Code.string_of_method request.meth in
    (* Nb the path will be url encoded as per spec *)
    let canoncical_uri = Compat.encode_string (Uri.path (uri request)) in
    (* Sort query string in alphabetical order by key *)
    let canonical_query = Compat.encode_query_string (uri request) in
    (* TODO: Merge identical headers *)
    let sorted_headers = Header.to_list request.headers
                         |> List.sort ~cmp:ksrt
                         |> List.map ~f:( fun (k, v) -> (String.lowercase k), (String.strip v))
    in
    let canonical_headers = sorted_headers
                            |> List.map ~f:(fun (k, v) -> sprintf "%s:%s\n" k v)
                            |> String.concat ~sep:""
    in
    let signed_headers = sorted_headers
                         |> List.map ~f:fst
                         |> String.concat ~sep:";"
    in
    let canonical_req = sprintf "%s\n%s\n%s\n%s\n%s\n%s"
        http_method canoncical_uri canonical_query canonical_headers signed_headers hashed_payload
    in
    (canonical_req, signed_headers)

  let string_to_sign ?time ~scope canonical_request:string =
    (* As per p. 23 of s3 api doc. The requests need current time in utc
       time parameter is there for testing. *)
    let time_str = match time with
        None -> Time.to_string_abs ~zone:Time.Zone.utc (Time.now())
      | Some t -> Compat.format_time t
    in
    let (scope_date, scope_region) = scope in
    let scope_str = sprintf "%s/%s/s3/aws4_request"
        (Date.to_string_iso8601_basic scope_date)
        (string_of_region scope_region)
    in
    let hashed_req = digest canonical_request in
    sprintf "AWS4-HMAC-SHA256\n%s\n%s\n%s" time_str scope_str hashed_req

  let make_signing_key ?date ~region ~secret_access_key =
    let date' = match date with
        None -> Date.today ~zone:Time.Zone.utc
      | Some d -> d in
    let date_str = Date.to_string_iso8601_basic date' in
    let date_key = mac (Cstruct.of_string ("AWS4"^secret_access_key)) date_str in
    let date_region_key = mac date_key (string_of_region region) in
    let date_region_service_key = mac date_region_key "s3" in
    let signing_key = mac date_region_service_key "aws4_request" in
    signing_key

  let auth_request ?now ~hashed_payload ~region ~aws_access_key ~aws_secret_key request =
    (* Important use the same time for everything here *)
    let time = Option.value ~default:(Time.now()) now in
    let date = Time.to_date ~zone:Time.Zone.utc time in
    let (canonical_request, signed_headers) = canonical_request hashed_payload request in
    let string_to_sign = string_to_sign ~time:time ~scope:(date, region) canonical_request in
    let signing_key = make_signing_key ~date ~region ~secret_access_key:aws_secret_key in
    let creds = sprintf "%s/%s/%s/s3/aws4_request"
        aws_access_key (Date.to_string_iso8601_basic date)
        (string_of_region region)
    in
    let signature = mac signing_key string_to_sign in

    let auth_header = sprintf
        "AWS4-HMAC-SHA256 Credential=%s,SignedHeaders=%s,Signature=%s"
        creds signed_headers (Compat.cstruct_to_hex_string signature)
    in
    [("Authorization", auth_header);]

end

let gzip_data ?level data =
  let write32 buf v =
    for i = 0 to 3 do
      Buffer.add_char buf (Char.of_int_exn (v lsr (i * 8) land 0xFF))
    done
  in
  let header = "\x1F\x8B\x08\x00\x00\x00\x00\x00\x00\xFF" in
  let len = String.length data in

  let compressed =
    Cryptokit.transform_string (Cryptokit.Zlib.compress ?level ()) data
  in
  let buffer = Buffer.create (len / 2) in
  Buffer.add_bytes buffer header;
  Buffer.add_bytes buffer compressed;
  let crc = (Crc.crc32 data |> Int63.to_int_exn) in
  write32 buffer crc;
  write32 buffer len;
  Buffer.contents buffer

let make_request ?body ?(region=`Us_east_1) ?(credentials:Credentials.t option) ~headers ~meth ~path ~query () =
  let host_str = region_host_string region in
  let uri = Uri.make
      ~scheme:"https"
      ~host:host_str
      ~path
      ~query:(List.map ~f:(fun (k,v) -> k, [v]) query)
      ()
  in
  let time = Time.now () in
  (* If PUT add content length *)
  let content_length = match meth with
    | `PUT ->
      let length = Option.value_map ~f:(String.length) ~default:0 body in
      Some ("Content-Length", Int.to_string length)
    | _ -> None
  in
  let host = "Host", host_str in
  let (amz_headers, hashed_payload) = Auth.make_amz_headers ?credentials time ?body in
  let headers =
    host :: (List.filter_opt [ content_length ]) @ headers @ amz_headers
  in

  let request = Request.make ~meth
      ~headers:(Header.of_list headers)
      uri in

  let auth_header =
    match credentials with
    | Some { Credentials.aws_access_key; aws_secret_key; _ } ->
        Auth.auth_request ~now:time
          ~hashed_payload ~region:region
          ~aws_access_key
          ~aws_secret_key request
    | None -> []
  in
  let headers = (headers @ auth_header) |> Header.of_list in
  let request = {request with Cohttp.Request.headers} in
  match meth with
  | `PUT -> Cohttp_async.Client.request
              ~body:(Option.value_map ~f:(Body.of_string) ~default:`Empty body)
              request
  | `GET -> Cohttp_async.Client.request request
  | `DELETE -> Cohttp_async.Client.request request
  | _ -> failwith "not possible right now"

(* Default sleep upto 400 seconds *)
let put ?(retries = 12) ?credentials ?(region=`Us_east_1) ?content_type ?(gzip=false) ?acl ?cache_control ~path data =
  let open Deferred.Or_error in

  let content_encoding, body = match gzip with
    | true -> Some "gzip", gzip_data data
    | false -> None, data
  in
  let rec cmd count =
    let open Async.Std in
    let headers =
      let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
      let content_encoding = Option.map ~f:(fun ct -> ("Content-Encoding", ct)) content_encoding in
      let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
      let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
      Core.Std.List.filter_opt [ content_type; content_encoding; cache_control; acl ]
    in
    make_request ?credentials ~region ~headers ~meth:`PUT ~path ~body ~query: [] () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        return (Ok ())
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Put %s was rate limited (%d). Sleeping %f ms" path code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to put s3://%s: Response was: %s" path body)
  in
  try_with_join (fun () -> cmd 0)

(* Default sleep upto 400 seconds *)
let get ?(retries = 12) ?credentials ?(region=`Us_east_1) ~path () =
  let rec cmd count =
    make_request ?credentials ~region ~headers:[] ~meth:`GET ~path ~query:[] () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        Body.to_string body >>= fun body ->
        return (Ok body)
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Get %s was rate limited (%d). Sleeping %f ms" path code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to get s3://%s. Error was: %s" path body)
  in
  Deferred.Or_error.try_with_join (fun () -> cmd 0)

(* Default sleep upto 400 seconds *)
let delete ?(retries = 12) ?credentials ?(region=`Us_east_1) ~path () =
  let rec cmd count =
    make_request ?credentials ~region ~headers:[] ~meth:`DELETE ~path ~query:[] () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        return (Ok ())
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Delete %s was rate limited (%d). Sleeping %f ms" path code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to delete s3://%s. Error was: %s" path body)
  in
  Deferred.Or_error.try_with_join (fun () -> cmd 0)

let delete_multi ?(retries = 12) ?credentials ?(region=`Us_east_1) ~bucket objects () =
  let body =
    Xml.Element
      ("Delete", [],
       List.map objects ~f:(fun path -> Xml.Element ("Object", [], [ Xml.Element ("Key", [], [ Xml.PCData path ]) ])))
    |> Xml.to_string
  in
  let headers = [ "Content-MD5", B64.encode (Digest.string body) ] in
  let rec cmd count =
    make_request ?credentials ~region ~headers ~meth:`POST ~query:["delete", ""] ~path:bucket () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        (* Decode the body, and return deleted objects *)


        return (Ok ())
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Multi delete on bucket '%s' was rate limited (%d). Sleeping %f ms" bucket code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to multi delete from bucket '%s'. Error was: %s" bucket body)
  in
  Deferred.Or_error.try_with_join (fun () -> cmd 0)

(** Find XML node *)
let rec get_value_exn name = function
  | [] -> raise Not_found
  | Xml.Element (n, _, [ Xml.PCData v ]) :: _ when n = name -> v
  | _ :: xs -> get_value_exn name xs

(** Find XML node *)
let rec get_value name = function
  | [] -> None
  | Xml.Element (n, _, [ Xml.PCData v ]) :: _ when n = name -> Some v
  | _ :: xs -> get_value name xs

(* Default sleep upto 400 seconds *)
type entry = { objekt: string;
                size: int;
             }

type ls_result = (entry list * ls_cont) Deferred.Or_error.t
and ls_cont = More of (unit -> ls_result) | Done

let rec ls ?(retries = 12) ?credentials ?(region=`Us_east_1) ?continuation_token ~path () : ls_result =
  let query =
    let q = [("list-type", "2")] in
    Option.value_map ~default:q ~f:(fun ct -> ("continuation-token", ct) :: q) continuation_token
  in
  let rec cmd count =
    make_request ?credentials ~region ~headers:[] ~meth:`GET ~path ~query () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        Body.to_string body >>= fun body ->
        (* Parse the xml result *)
        let xml = Xml.parse_string body |> Xml.children in
        let elements =
          xml
          |> List.filter_map ~f:( function Xml.Element ("Contents", _, v) -> Some v | _ -> None )
          |> List.map ~f:( fun xml -> { objekt = get_value_exn "Key" xml;
                                        size = get_value_exn "Size" xml |> int_of_string} )
        in
        let continuation = match get_value "NextContinuationToken" xml with
          | Some ct -> More (ls ~retries ?credentials ~region ~continuation_token:ct ~path)
          | None -> Done
        in
        return (Ok (elements, continuation))
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Get %s was rate limited (%d). Sleeping %f ms" path code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to ls s3://%s. Error was: %s" path body)
  in
  Deferred.Or_error.try_with_join (fun () -> cmd 0)

module Test = struct
  open OUnit2

  let async f ctx =
    Thread_safe.block_on_async_exn (fun () -> f ctx)

  open Async.Std

  let assert_ok ~msg = function
    | Ok r -> r
    | Error e -> assert_failure (msg ^ ": " ^ (Core_kernel.Error.to_string_hum e))

  let gunzip data =
    Process.create ~prog:"gunzip" ~args:[ "--no-name"; "-" ] () >>= fun proc ->
    let proc = Or_error.ok_exn proc in
    (* Write to the process. *)
    Writer.write (Process.stdin proc) data;
    Process.collect_stdout_and_wait proc

  let gzip ?(level=6) data =
    Process.create ~prog:"gzip" ~args:[ sprintf "-%d" level; "--no-name"; "-" ] () >>= fun proc ->
    let proc = Or_error.ok_exn proc in
    (* Write to the process. *)
    Writer.write (Process.stdin proc) data;
    Process.collect_stdout_and_wait proc

  let test_gzip _ =
    let test len =
      let string = String.init len ~f:(fun _ -> Char.of_int_exn (Random.int 8)) in
      let gzipped = gzip_data ~level:9 string in
      gunzip gzipped >>= fun gunzipped ->
      assert_equal string (Or_error.ok_exn gunzipped);
      return ()
    in

    List.init ~f:(fun _ -> Random.int 100_000) 100
    |> Deferred.List.iter ~how:`Parallel ~f:(test)

  let unit_test =
    "s3" >::: [
      "gzip" >:: async test_gzip
    ]

end
