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
open Core
open Cohttp
open Protocol_conv_xml

(* Protocol definitions *)
module Protocol(P: sig type 'a result end) = struct
  type time = Core.Time.t
  let time_of_xml_light t =
    Xml_light.to_string t |> Time.of_string

  type etag = string [@@deriving protocol ~driver:(module Xml_light)]
  module Ls = struct

    type storage_class =
      | Standard           [@key "STANDARD"]
      | Standard_ia        [@key "STANDARD_IA"]
      | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
      | Glacier            [@key "GLACIER"]

    and content = {
      storage_class: storage_class [@key "StorageClass"];
      size: int [@key "Size"];
      last_modified: time [@key "LastModified"];
      key: string [@key "Key"];
      etag: etag [@key "ETag"];
    }

    and result = {
      prefix: string option [@key "Prefix"];
      common_prefixes: string option [@key "CommonPrefixes"];
      delimiter: string option [@key "Delimiter"];
      next_continuation_token: string option [@key "NextContinuationToken"];
      name: string [@key "Name"];
      max_keys: int [@key "MaxKeys"];
      key_count: int [@key "KeyCount"];
      is_truncated: bool [@key "IsTruncated"];
      contents: content list [@key "Contents"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type t = (content list * cont) P.result
    and cont = More of (unit -> t) | Done

  end

  let set_element_name name = function
    | Xml.Element (_, attribs, ts) -> Xml.Element (name, attribs, ts)
    | _ -> failwith "Not an element"

  module Delete_multi = struct
    type objekt = {
      key: string [@key "Key"];
      version_id: string option [@key "VersionId"];
    }
    [@@deriving protocol ~driver:(module Xml_light)]

    type request = {
      quiet: bool [@key "Quiet"];
      objects: objekt list [@key "Object"]
    } [@@deriving protocol ~driver:(module Xml_light)]

    let xml_of_request request =
      request_to_xml_light request |> set_element_name "Delete"

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
      deleted: objekt list [@key "Deleted"];
      error: error list  [@key "Error"];
    } [@@deriving protocol ~driver:(module Xml_light)]

  end

  module Error_response = struct
    type t = {
      code: string [@key "Code"];
      message: string [@key "Message"];
      bucket: string option [@key "Bucket"];
      endpoint: string option [@key "Endpoint"];
      region: string option [@key "Region"];
      request_id: string [@key "RequestId"];
      host_id: string [@key "HostId"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Multipart = struct
    type part = { part_number: int [@key "PartNumber"];
                  etag: etag [@key "ETag"];
                }
    [@@deriving to_protocol ~driver:(module Xml_light)]

    module Initiate = struct
      type t = {
        bucket: string [@key "Bucket"];
        key: string [@key "Key"];
        upload_id: string [@key "UploadId"];
      } [@@deriving of_protocol ~driver:(module Xml_light)]
    end
    module Complete = struct
      type request = {
        parts: part list [@key "Part"];
      }
      [@@deriving to_protocol ~driver:(module Xml_light)]
      let xml_of_request request =
        request_to_xml_light request |> set_element_name "CompleteMultipartUpload"
      type response = { location: string [@key "Location"];
                        bucket: string [@key "Bucket"];
                        key: string [@key "Key"];
                        etag: etag [@key "ETag"];
                      }
      [@@deriving of_protocol ~driver:(module Xml_light)]
    end
    module Copy = struct
      type t = {
        etag: etag [@key "ETag"];
        last_modified: time [@key "LastModified"];
      }
      [@@deriving of_protocol ~driver:(module Xml_light)]
    end
  end
end

module Make(Compat : Types.Compat) = struct
  module Util_deferred = Util.Make(Compat)
  open Compat
  open Deferred.Infix

  type error =
    | Redirect of Util.region
    | Throttled
    | Unknown of int * string
    | Not_found

  include Protocol(struct type nonrec 'a result = ('a, error) result Deferred.t end)

  type range = { first: int option; last: int option }

  type nonrec 'a result = ('a, error) result Deferred.t
  type 'a command = ?scheme:[`Http|`Https] -> ?credentials:Credentials.t -> ?region:Util.region -> 'a

  (**/**)
  let do_command ?region cmd =
    cmd ?region () >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    match Code.code_of_status status with
    | code when 200 <= code && code < 300 ->
      let headers = Cohttp.Response.headers resp in
      Deferred.return (Ok (headers, body))
    | 404 -> Deferred.return (Error (Not_found))
    | c when 300 <= c && c < 500 -> begin
        let open Error_response in
        Cohttp_deferred.Body.to_string body >>= fun body ->
        let xml = Xml.parse_string body in
        match Error_response.t_of_xml_light xml with
        | { code = "PermanentRedirect"; endpoint = Some host; _ }
        | { code = "TemporaryRedirect"; endpoint = Some host; _ } ->
          let region = Util.region_of_host host in
          Deferred.return (Error (Redirect region))
        | { code = "AuthorizationHeaderMalformed"; region = Some region; _ } ->
          let region = Util.region_of_string region in
          Deferred.return (Error (Redirect region))
        | { code; _ } ->
          Deferred.return (Error (Unknown (c, code)))
      end
    | (500 | 503) ->
      (* 500, InternalError | 503, SlowDown | 503, ServiceUnavailable -> Throttle *)
      Deferred.return (Error Throttled)
    | code ->
      Cohttp_deferred.Body.to_string body >>= fun body ->
      let resp = Error_response.t_of_xml_light (Xml.parse_string body) in
      Deferred.return (Error (Unknown (code, resp.code)))
  (**/**)

  (** Upload a file to S3. *)
  let put ?scheme ?credentials ?region ?content_type ?content_encoding ?acl ?cache_control ~bucket ~key ~data () =
    let scheme = Option.value ~default:`Http scheme in
    let path = sprintf "%s/%s" bucket key in
    let headers =
      let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
      let content_encoding = Option.map ~f:(fun ct -> ("Content-Encoding", ct)) content_encoding in
      let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
      let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
      Core.List.filter_opt [ content_type; content_encoding; cache_control; acl ]
    in
    let cmd ?region () =
      Util_deferred.make_request ~scheme ?credentials ?region ~headers ~meth:`PUT ~path ~body:data ~query:[] ()
    in

    do_command ?region cmd >>=? fun (headers, _body) ->
    let etag =
      Header.get headers "etag"
      |> (fun etag -> Option.value_exn ~message:"Put reply did not conatin an etag header" etag)
      |> String.strip ~drop:(function '"' -> true | _ -> false)
    in
    Deferred.return (Ok etag)

  (** Retrieve a file from s3. The range option can be specified to only retrieve a part of the file.*)
  let get ?scheme ?credentials ?region ?range ~bucket ~key () =
    let scheme = Option.value ~default:`Http scheme in

    let headers =
      let r_opt r = Option.value_map ~f:(string_of_int) ~default:"" r in

      [ Option.bind ~f:(function { first = None; last = None }-> None
                               | { first = Some first; last } ->
                                 Some ("Range", sprintf "bytes=%d-%s" first (r_opt last))
                               | { first = None; last = Some last } when last < 0 ->
                                 Some ("Range", sprintf "bytes=-%d" last)
                               | { first = None; last = Some last } ->
                                 Some ("Range", sprintf "bytes=0-%d" last)

          ) range
      ]
      |> List.filter_opt
    in
    let path = sprintf "%s/%s" bucket key in
    let cmd ?region () =
      Util_deferred.make_request ~scheme ?credentials ?region ~headers ~meth:`GET ~path ~query:[] ()
    in
    do_command ?region cmd >>=? fun (_headers, body) ->
    Cohttp_deferred.Body.to_string body >>= fun body ->
    Deferred.return (Ok body)

  (* Delete a single file on S3 *)
  let delete ?scheme ?credentials ?region ~bucket ~key () =
    let scheme = Option.value ~default:`Http scheme in
    let path = sprintf "%s/%s" bucket key in
    let cmd ?region () =
      Util_deferred.make_request ~scheme ?credentials ?region ~headers:[] ~meth:`DELETE ~path ~query:[] ()
    in
    do_command ?region cmd >>=? fun (_headers, _body) ->
    Deferred.return (Ok ())

  (* Delete a list of objects from s3, all residing in the same bucket *)
  let delete_multi ?scheme ?credentials ?region ~bucket ~objects () =
    let scheme = Option.value ~default:`Http scheme in
    match objects with
    | [] -> Delete_multi.{
        delete_marker = false;
        delete_marker_version_id = None;
        deleted = [];
        error = [];
      } |> (fun r -> Deferred.return (Ok r))
    | _ ->
      let request =
        Delete_multi.{
            quiet=false;
            objects=objects;
          }
        |> Delete_multi.xml_of_request
        |> Xml.to_string
      in
      let headers = [ "Content-MD5", B64.encode (Caml.Digest.string request) ] in
      let cmd ?region () =
        Util_deferred.make_request ~scheme
          ~body:request ?credentials ?region ~headers
          ~meth:`POST ~query:["delete", ""] ~path:bucket ()
      in
      do_command ?region cmd >>=? fun (_headers, body) ->
      Cohttp_deferred.Body.to_string body >>= fun body ->
      let result = Delete_multi.result_of_xml_light (Xml.parse_string body) in
      Deferred.return (Ok result)

  (** List contents of bucket in s3. *)
  let rec ls ?scheme ?credentials ?region ?continuation_token ?prefix ~bucket () =
    let scheme = Option.value ~default:`Http scheme in
    let query = [ Some ("list-type", "2");
                  Option.map ~f:(fun ct -> ("continuation-token", ct)) continuation_token;
                  Option.map ~f:(fun prefix -> ("prefix", prefix)) prefix;
                ] |> List.filter_opt
    in
    let cmd ?region () =
      Util_deferred.make_request ~scheme ?credentials ?region ~headers:[] ~meth:`GET ~path:bucket ~query ()
    in
    do_command ?region cmd >>=? fun (_headers, body) ->
    Cohttp_deferred.Body.to_string body >>= fun body ->
    let result = Ls.result_of_xml_light (Xml.parse_string body) in
    let continuation = match Ls.(result.next_continuation_token) with
      | Some ct -> Ls.More (ls ~scheme ?credentials ?region ~continuation_token:ct ?prefix ~bucket)
      | None -> Ls.Done
    in
    Deferred.return (Ok (Ls.(result.contents, continuation)))

  (** Function for doing multipart uploads *)
  module Multipart_upload = struct
    type t = { id: string;
               mutable parts: Multipart.part list;
               bucket: string;
               key: string;
             }

    (** Initiate a multipart upload *)
    let init ?scheme ?credentials ?region ?content_type ?content_encoding ?acl ?cache_control ~bucket ~key  () =
      let scheme = Option.value ~default:`Http scheme in
      let path = sprintf "%s/%s" bucket key in
      let query = ["uploads", ""] in
      let headers =
        let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
        let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
        let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
        Core.List.filter_opt [ content_type; content_encoding; cache_control; acl ]
      in
      let cmd ?region () =
        Util_deferred.make_request ~scheme ?credentials ?region ~headers ~meth:`POST ~path ~query ()
      in

      do_command ?region cmd >>=? fun (_headers, body) ->
      Cohttp_deferred.Body.to_string body >>| fun body ->
      let resp = Multipart.Initiate.of_xml_light (Xml.parse_string body) in
      Ok { id = resp.Multipart.Initiate.upload_id;
           parts = [];
           bucket;
           key;
         }

    (** Upload a part of the file.
        Parts must be at least 5Mb except for the last part
        [part_number] specifies the part numer. Parts will be assembled in order, but
        does not have to be consequtive
    *)
    let upload_part ?scheme ?credentials ?region t ~part_number ~data () =
      let scheme = Option.value ~default:`Http scheme in
      let path = sprintf "%s/%s" t.bucket t.key in
      let query =
        [ "partNumber", string_of_int part_number;
          "uploadId", t.id ]
      in
      let cmd ?region () =
        Util_deferred.make_request ~scheme ?credentials ?region ~headers:[] ~meth:`PUT ~path ~body:data ~query ()
      in

      do_command ?region cmd >>=? fun (headers, _body) ->
      let etag =
        Header.get headers "etag"
        |> (fun etag -> Option.value_exn ~message:"Put reply did not conatin an etag header" etag)
        |> String.strip ~drop:(function '"' -> true | _ -> false)
      in
      t.parts <- { etag; part_number } :: t.parts;
      Deferred.return (Ok ())

    (** Specify a part to be a file on s3.
        [range] can be used to only include a part of the s3 file
    *)
    let copy_part ?scheme ?credentials ?region t ~part_number ?range ~bucket ~key () =
      let scheme = Option.value ~default:`Http scheme in
      let path = sprintf "%s/%s" t.bucket t.key in
      let query =
        [ "partNumber", string_of_int part_number;
          "uploadId", t.id ]
      in
      let headers =
        ("x-amz-copy-source", sprintf "/%s/%s" bucket key) ::
        Option.value_map ~default:[] ~f:(fun (first, last) ->
            [ "x-amz-copy-source-range", sprintf "bytes=%d-%d" first last ]) range
      in
      let cmd ?region () =
        Util_deferred.make_request ~scheme ?credentials ?region ~headers ~meth:`PUT ~path ~query ()
      in

      do_command ?region cmd >>=? fun (_headers, body) ->
      Cohttp_deferred.Body.to_string body >>| fun body ->
      let xml = Xml.parse_string body in
      match Multipart.Copy.of_xml_light xml with
      | { Multipart.Copy.etag; _ } ->
        t.parts <- { etag; part_number } :: t.parts;
        (Ok ())

    (** Complete the multipart upload.
        The returned etag is a opaque identifier (not md5)
    *)
    let complete ?scheme ?credentials ?region t () =
      let scheme = Option.value ~default:`Http scheme in
      let path = sprintf "%s/%s" t.bucket t.key in
      let query = [ "uploadId", t.id ] in
      let request =
        (* TODO: Sort the parts by partNumber *)
        let parts = Caml.List.sort (fun a b -> compare a.Multipart.part_number b.part_number) t.parts in
        Multipart.Complete.(xml_of_request { parts })
        |> Xml.to_string_fmt
      in
      let cmd ?region () =
        Util_deferred.make_request ~scheme ?credentials ?region ~headers:[] ~meth:`POST ~path ~query ~body:request ()
      in
      do_command ?region cmd >>=? fun (_headers, body) ->
      Cohttp_deferred.Body.to_string body >>| fun body ->
      let xml = Xml.parse_string body in
      match Multipart.Complete.response_of_xml_light xml with
      | { location=_; etag; bucket; key } when bucket = t.bucket && key = t.key ->
        Ok etag
      | _ -> Error (Unknown ((-1), "Bucket/key does not match"))

    (** Abort a multipart upload, deleting all specified parts *)
    let abort ?scheme ?credentials ?region t () =
      let scheme = Option.value ~default:`Http scheme in
      let path = sprintf "%s/%s" t.bucket t.key in
      let query = [ "uploadId", t.id ] in
      let cmd ?region () =
        Util_deferred.make_request ~scheme ?credentials ?region ~headers:[] ~meth:`DELETE ~path ~query ()
      in
      do_command ?region cmd >>=? fun (_headers, _body) ->
      Deferred.return (Ok ())
  end
  (** Helper function to handle redirects and throttling *)

  let retry ?region ~retries ~f () =
    let delay n =
      let jitter = Random.float 0.5 +. 0.5 in
      let backoff = 2.0 ** (float n) in
      (min 60.0 backoff) *. jitter
      in
    let rec inner ?region ~retry_count ~redirected () =
      f ?region () >>= function
      | Error (Redirect _) as e when redirected ->
        Deferred.return e
      | Error (Redirect region) ->
        inner ~region ~retry_count ~redirected:true ()
      | Error _ as e when retry_count = retries ->
        Deferred.return e
      | Error (Throttled) ->
        Deferred.after (delay (retry_count + 1)) >>= fun () ->
        inner ?region ~retry_count:(retry_count + 1) ~redirected ()
      | Error _ ->
        inner ?region ~retry_count:(retry_count + 1) ~redirected ()
      | Ok r -> Deferred.return (Ok r)
    in
    inner ?region ~retry_count:0 ~redirected:false ()
end

module Test = struct
  open OUnit2

  module Protocol = Protocol(struct type 'a result = 'a end)

  let parse_result _ =
    let data = {|
      <ListBucketResult>
        <Name>s3_osd</Name>
        <Prefix></Prefix>
        <KeyCount>1</KeyCount>
        <MaxKeys>1000</MaxKeys>
        <IsTruncated>false</IsTruncated>
        <Contents>
          <StorageClass>STANDARD</StorageClass>
          <Key>test</Key>
          <LastModified>2018-02-27T13:39:35.000Z</LastModified>
          <ETag>&quot;7538d2bd85ea5dfb689ed65a0f60a7cf&quot;</ETag>
          <Size>20</Size>
        </Contents>
        <Contents>
          <StorageClass>STANDARD</StorageClass>
          <Key>test</Key>
          <LastModified>2018-02-27T13:39:35.000Z</LastModified>
          <ETag>&quot;7538d2bd85ea5dfb689ed65a0f60a7cf&quot;</ETag>
          <Size>20</Size>
        </Contents>
      </ListBucketResult>
      |}
    in
    let xml = Xml.parse_string data in
    let result = Protocol.Ls.result_of_xml_light xml in
    ignore result;
    ()

  let parse_error _ =
    let data =
      {| <Error>
           <Code>PermanentRedirect</Code>
           <Message>The bucket you are attempting to access must be addressed using the specified endpoint. Please send all future requests to this endpoint.</Message>
           <Bucket>stijntest</Bucket>
           <Endpoint>stijntest.s3.amazonaws.com</Endpoint>
           <RequestId>9E23E3919C24476C</RequestId>
           <HostId>zdRmjNUli+pR+gwwhfGt2/s7VVerHquAPqgi9KpZ9OVsYhfF+9uAkkRJtxPcLCJKk2ZjzV1MTv8=</HostId>
         </Error>
      |}
    in
    let xml = Xml.parse_string data in
    let error = Protocol.Error_response.t_of_xml_light xml in
    assert_equal ~msg:"Wrong code extracted" "PermanentRedirect" (error.Protocol.Error_response.code);
    ()

  let unit_test =
    __MODULE__ >::: [
      "parse_result" >:: parse_result;
      "parse_error" >:: parse_error;
    ]
end
