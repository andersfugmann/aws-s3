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
open Core
open Async
open Cohttp
open Cohttp_async
open Protocol_conv_xml

type 'a command = ?retries:int -> ?credentials:Credentials.t -> ?region:Util.region -> 'a


module Ls = struct
  type time = Time.t
  let time_of_xml_light t =
    Xml_light.to_string t |> Time.of_string

  type storage_class = Standard [@key "STANDARD"]
                     | Standard_ia [@key "STANDARD_IA"]
                     | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
                     | Glacier [@key "GLACIER"]
  [@@deriving of_protocol ~driver:(module Xml_light)]

  type contents = {
    storage_class: storage_class [@key "StorageClass"];
    size: int [@key "Size"];
    last_modified: time [@key "LastModified"];
    key: string [@key "Key"];
    etag: string [@key "ETag"];
  } [@@deriving of_protocol ~driver:(module Xml_light)]


  type result = {
    prefix: string option [@key "Prefix"];
    common_prefixes: string option [@key "CommonPrefixes"];
    delimiter: string option [@key "Delimiter"];
    next_continuation_token: string option [@key "NextContinuationToken"];
    name: string [@key "Name"];
    max_keys: int [@key "MaxKeys"];
    key_count: int [@key "KeyCount"];
    is_truncated: bool [@key "IsTruncated"];
    contents: contents list [@key "Contents"];
  } [@@deriving of_protocol ~driver:(module Xml_light)]

  type t = (contents list * cont) Deferred.Or_error.t
  and cont = More of (unit -> t) | Done

end

let set_element_name name = function
  | Xml.Element (_, attribs, ts) -> Xml.Element (name, attribs, ts)
  | _ -> failwith "Not an element"

module Delete_multi = struct
  type objekt = {
    key: string [@key "Object"];
    version_id: string option [@key "VersionId"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type request = {
    quiet: bool [@key "Quiet"];
    objects: objekt list [@key "Object"]
  } [@@deriving protocol ~driver:(module Xml_light)]

  let xml_of_request request =
    request_to_xml_light request |> set_element_name "Delete"

  type deleted = {
    key: string [@key "Key"];
    version_id: string option [@key "VersionId"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type error = {
    key: string [@key "Key"];
    version_id: string option [@key "VersionId"];
    code: string;
    message : string;
  } [@@deriving protocol ~driver:(module Xml_light)]

  type delete_marker = bool

  let delete_marker_of_xml_light t =
    Xml_light.to_option (Xml_light.to_bool) t
    |> function None -> false
              | Some x -> x

  let delete_marker_to_xml_light _t = failwith "Not implemented"

  type result = {
    delete_marker: delete_marker [@key "DeleteMarker"];
    delete_marker_version_id: string option [@key "DeleteMarkerVersionId"];
    deleted: deleted list [@key "Deleted"];
    error: error list  [@key "Error"];
  } [@@deriving protocol ~driver:(module Xml_light)]

end


(* Default sleep upto 400 seconds *)
let put ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ?content_type ?(gzip=false) ?acl ?cache_control ~bucket ~key data =
  let open Deferred.Or_error in
  let path = sprintf "%s/%s" bucket key in
  let content_encoding, body = match gzip with
    | true -> Some "gzip", Util.gzip_data data
    | false -> None, data
  in
  let rec cmd count =
    let open Async in
    let headers =
      let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
      let content_encoding = Option.map ~f:(fun ct -> ("Content-Encoding", ct)) content_encoding in
      let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
      let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
      Core.List.filter_opt [ content_type; content_encoding; cache_control; acl ]
    in
    Util.make_request ?credentials ~region ~headers ~meth:`PUT ~path ~body ~query: [] () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        return (Ok ())
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Put s3://%s was rate limited (%d). Sleeping %f ms" path code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to put s3://%s: Response was: %s" path body)
  in
  try_with_join (fun () -> cmd 0)

(* Default sleep upto 400 seconds *)
let get ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ~bucket ~key () =
  let path = sprintf "%s/%s" bucket key in
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers:[] ~meth:`GET ~path:(bucket ^ "/" ^ key) ~query:[] () >>= fun (resp, body) ->
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

let delete ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ~bucket ~key () =
  let path = sprintf "%s/%s" bucket key in
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
  let request =
    Delete_multi.({
        quiet=false;
        objects=objects;
      })
    |> Delete_multi.xml_of_request
    |> Xml.to_string
  in
  let headers = [ "Content-MD5", B64.encode (Digest.string request) ] in
  let rec cmd count =
    Util.make_request ~body:request ?credentials ~region ~headers ~meth:`POST ~query:["delete", ""] ~path:bucket () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        Body.to_string body >>= fun body ->
        let result = Delete_multi.result_of_xml_light (Xml.parse_string body) in
        return (Ok result)
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

let rec ls ?(retries = 12) ?credentials ?(region=Util.Us_east_1) ?continuation_token ?prefix ~bucket () : Ls.t =
  let query = [ Some ("list-type", "2");
                Option.map ~f:(fun ct -> ("continuation-token", ct)) continuation_token;
                Option.map ~f:(fun prefix -> ("prefix", prefix)) prefix;
              ] |> List.filter_opt
  in
  let rec cmd count =
    Util.make_request ?credentials ~region ~headers:[] ~meth:`GET ~path:bucket ~query () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match status, Code.code_of_status status with
    | #Code.success_status, _ ->
        Body.to_string body >>= fun body ->
        let result = Ls.result_of_xml_light (Xml.parse_string body) in
        let continuation = match Ls.(result.next_continuation_token) with
          | Some ct -> Ls.More (ls ~retries ?credentials ~region ~continuation_token:ct ?prefix ~bucket)
          | None -> Ls.Done
        in
        return (Ok (Ls.(result.contents, continuation)))
    | _, ((500 | 503) as code) when count < retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) *. 100.) in
        Log.Global.info "Get %s was rate limited (%d). Sleeping %f ms" bucket code delay;
        after (Time.Span.of_ms delay) >>= fun () ->
        cmd (count + 1)
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to ls s3://%s. Error was: %s" bucket body)
  in
  Deferred.Or_error.try_with_join (fun () -> cmd 0)
