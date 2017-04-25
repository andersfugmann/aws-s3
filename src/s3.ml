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

module R = Result
open Core.Std
open Async.Std
open Cohttp
open Cohttp_async

module Ls_result = struct
  type time = Time.t
  let time_of_yojson = function
    | `String s -> Pervasives.Ok (Time.of_string s)
    | _ -> Pervasives.Error "Expected string"

  type storage_class = Standard | Standard_ia | Reduced_redundancy | Glacier
  let storage_class_of_yojson = function
    | `String "STANDARD" -> Pervasives.Ok Standard
    | `String "STANDARD_IA" -> Pervasives.Ok Standard_ia
    | `String "REDUCED_REDUNDANCY" -> Pervasives.Ok Reduced_redundancy
    | `String "GLACIER" -> Pervasives.Ok Glacier
    | _ -> Pervasives.Error "Expected string"

  type contents = {
    storage_class: storage_class [@key "StorageClass"];
    size: int [@key "Size"];
    last_modified: time [@key "LastModified"];
    key: string [@key "Key"];
    etag: string [@key "ETag"];
  } [@@deriving of_yojson { strict = false }]

  type list_bucket_result = {
    prefix: string option [@key "Prefix"];
    next_continuation_token: string option [@key "NextContinuationToken"] [@default None];
    name: string [@key "Name"];
    max_keys: int [@key "MaxKeys"];
    key_count: int [@key "KeyCount"];
    is_truncated: bool [@key "IsTruncated"];
    contents: contents list [@key "Contents"];
  } [@@deriving of_yojson { strict = false }]

  type t = {
    result: list_bucket_result [@key "ListBucketResult"];
  } [@@deriving of_yojson { strict = false }]

  let of_xml s =
    Xml.parse_string s
               |> Util.yojson_of_xml
               |> of_yojson
               |> function | R.Ok t -> t
                           | R.Error s -> failwith s

  type result = (contents list * cont) Deferred.Or_error.t
  and cont = More of (unit -> result) | Done

end

(* Default sleep upto 400 seconds *)
let put ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ?content_type ?(gzip=false) ?acl ?cache_control ~path data =
  let open Deferred.Or_error in

  let content_encoding, body = match gzip with
    | true -> Some "gzip", Util.gzip_data data
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
    Util.make_request ?credentials ~region ~headers ~meth:`PUT ~path ~body ~query: [] () >>= fun (resp, body) ->
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
let get ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ~path () =
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers:[] ~meth:`GET ~path ~query:[] () >>= fun (resp, body) ->
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
let delete ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ~path () =
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers:[] ~meth:`DELETE ~path ~query:[] () >>= fun (resp, body) ->
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

let delete_multi ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ~bucket objects () =
  let body =
    Xml.Element
      ("Delete", [],
       List.map objects ~f:(fun path -> Xml.Element ("Object", [], [ Xml.Element ("Key", [], [ Xml.PCData path ]) ])))
    |> Xml.to_string
  in
  let headers = [ "Content-MD5", B64.encode (Digest.string body) ] in
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers ~meth:`POST ~query:["delete", ""] ~path:bucket () >>= fun (resp, body) ->
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

let rec ls ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ?continuation_token ~path () : Ls_result.result =
  let query =
    let q = [("list-type", "2")] in
    Option.value_map ~default:q ~f:(fun ct -> ("continuation-token", ct) :: q) continuation_token
  in
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers:[] ~meth:`GET ~path ~query () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        Body.to_string body >>= fun body ->
        (* Parse the xml result *)
        let result = Ls_result.of_xml body in
        let continuation = match Ls_result.(result.result.next_continuation_token) with
          | Some ct -> Ls_result.More (ls ~retries ?credentials ~region ~continuation_token:ct ~path)
          | None -> Ls_result.Done
        in
        return (Ok (Ls_result.(result.result.contents, continuation)))
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
