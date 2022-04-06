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
open StdLabels
let sprintf = Printf.sprintf

let xmlm_of_string string =
  let (_dtd, nodes) = Ezxmlm.from_string string in
  List.hd nodes

let make_xmlm_node ?(ns = "") name attrs nodes : 'a Xmlm.frag =
  `El (((ns, name), attrs), nodes)

module Option = struct
  let map ?default ~f = function
    | None -> default
    | Some v -> Some (f v)

  let value ~default = function
    | None -> default
    | Some v -> v

  let value_map ~default ~f v =
    map ~f v
    |> value ~default

  let value_exn ~message = function
    | Some v -> v
    | None -> failwith message

end

let rec filter_map ~f = function
  | [] -> []
  | x :: xs -> begin
      match f x with
      | Some x -> x :: (filter_map ~f xs)
      | None -> (filter_map ~f xs)
    end


(* Protocol definitions *)
module Protocol(P: sig type 'a result end) = struct
  type time = float
  let time_of_xmlm_exn t =
    try
      Protocol_conv_xmlm.Xmlm.to_string t |> Time.parse_iso8601_string
    with
    | _ -> raise Protocol_conv_xmlm.Xmlm.(Protocol_error (make_error ~value:t "Not an iso8601 string"))

  let unquote s =
    match String.length s with
    | 0 | 1 -> s
    | _ when s.[0] = '"' ->
      String.sub s ~pos:1 ~len:(String.length s - 2)
    | _ -> s

  type etag = string
  let etag_of_xmlm_exn t =
    Protocol_conv_xmlm.Xmlm.to_string t |> unquote

  type storage_class =
    | Standard           [@key "STANDARD"]
    | Standard_ia        [@key "STANDARD_IA"]
    | Onezone_ia         [@key "ONEZONE_IA"]
    | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
    | Glacier            [@key "GLACIER"]
  and content = {
    storage_class: storage_class [@key "StorageClass"];
    size: int [@key "Size"];
    last_modified: time [@key "LastModified"];
    key: string [@key "Key"];
    etag: etag [@key "ETag"];
    meta_headers: (string * string) list option; [@default None]
    (** Add expiration date option *)
  } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  module Ls = struct

    type result = {
      prefix: string option [@key "Prefix"];
      common_prefixes: string option [@key "CommonPrefixes"];
      delimiter: string option [@key "Delimiter"];
      next_continuation_token: string option [@key "NextContinuationToken"];
      name: string [@key "Name"];
      max_keys: int [@key "MaxKeys"];
      key_count: int [@key "KeyCount"];
      is_truncated: bool [@key "IsTruncated"];
      contents: content list [@key "Contents"];
      start_after: string option [@key "StartAfter"];
    } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

    type t = (content list * cont) P.result
    and cont = More of (?max_keys:int -> unit -> t) | Done

  end

  let set_element_name: string -> 'a Xmlm.frag -> 'a Xmlm.frag = fun name -> function
    | `El (((ns, _), attrs), elems) ->
      make_xmlm_node ~ns name attrs elems
    | `Data _ -> failwith "Not an element"

  module Delete_multi = struct
    type objekt = {
      key: string [@key "Key"];
      version_id: string option [@key "VersionId"];
    } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

    (** We must not transmit the version id at all if not specified *)
    let objekt_to_xmlm = function
      | { key; version_id = None } ->
        make_xmlm_node "object" [] [ make_xmlm_node "Key" [] [`Data key] ]
      | { key; version_id = Some version } ->
        make_xmlm_node "object" [] [
          make_xmlm_node "Key" [] [`Data key];
          make_xmlm_node "VersionId" [] [`Data version];
        ]

    type request = {
      quiet: bool [@key "Quiet"];
      objects: objekt list [@key "Object"]
    } [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

    let xml_of_request request =
      request_to_xmlm request |> set_element_name "Delete"

    type error = {
      key: string [@key "Key"];
      version_id: string option [@key "VersionId"];
      code: string [@key "Code"];
      message : string [@key "Message"];
    } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

    type delete_marker = bool

    let delete_marker_of_xmlm_exn t =
      Protocol_conv_xmlm.Xmlm.to_option (Protocol_conv_xmlm.Xmlm.to_bool) t
      |> function None -> false
                | Some x -> x

    type result = {
      delete_marker: delete_marker [@key "DeleteMarker"];
      delete_marker_version_id: string option [@key "DeleteMarkerVersionId"];
      deleted: objekt list [@key "Deleted"];
      error: error list  [@key "Error"];
    } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

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
    } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
  end

  module Multipart = struct
    type part = { part_number: int [@key "PartNumber"];
                  etag: string [@key "ETag"];
                }
    [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

    module Initiate = struct
      type t = {
        bucket: string [@key "Bucket"];
        key: string [@key "Key"];
        upload_id: string [@key "UploadId"];
      } [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
    end
    module Complete = struct
      type request = {
        parts: part list [@key "Part"];
      }
      [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
      let xml_of_request request =
        request_to_xmlm request |> set_element_name "CompleteMultipartUpload"
      type response = { location: string [@key "Location"];
                        bucket: string [@key "Bucket"];
                        key: string [@key "Key"];
                        etag: string [@key "ETag"];
                      }
      [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
    end
    module Copy = struct
      type t = {
        etag: string [@key "ETag"];
        last_modified: time [@key "LastModified"];
      }
      [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
    end
  end
end

module Make(Io : Types.Io) = struct
  module Aws = Aws.Make(Io)
  module Body = Body.Make(Io)
  open Io
  open Deferred
  type error =
    | Redirect of Region.endpoint
    | Throttled
    | Unknown of int * string
    | Failed of exn
    | Forbidden
    | Not_found

  let string_sink () =
    let reader, writer = Pipe.create () in
    Body.to_string reader, writer

  include Protocol(struct type nonrec 'a result = ('a, error) result Deferred.t end)

  type range = { first: int option; last: int option }

  type nonrec 'a result = ('a, error) result Deferred.t
  type 'a command = ?credentials:Credentials.t -> ?connect_timeout_ms:int -> endpoint:Region.endpoint -> 'a

  (**/**)
  let do_command ~(endpoint:Region.endpoint) cmd =
    cmd () >>=
    (function Ok v -> return (Ok v) | Error exn -> return (Error (Failed exn))) >>=? fun (code, _message, headers, body) ->
    match code with
    | code when 200 <= code && code < 300 ->
      Deferred.return (Ok headers)
    | 403 -> Deferred.return (Error Forbidden)
    | 404 -> Deferred.return (Error Not_found)
    | c when 300 <= c && c < 400 ->
      (* Redirect of sorts *)
      let region =
        Region.of_string
          (Headers.find "x-amz-bucket-region" headers)
      in
      Deferred.return (Error (Redirect {endpoint with region}))
    | c when 400 <= c && c < 500 -> begin
        let open Error_response in
        let xml = xmlm_of_string body in
        match Error_response.of_xmlm_exn xml with
        | { code = "PermanentRedirect"; endpoint = Some host; _ }
        | { code = "TemporaryRedirect"; endpoint = Some host; _ } ->
          let region = Region.of_host host in
          Deferred.return (Error (Redirect {endpoint with region}))
        | { code = "AuthorizationHeaderMalformed"; region = Some region; _ } ->
          let region = Region.of_string region in
          Deferred.return (Error (Redirect {endpoint with region}))
        | { code; _ } ->
          Deferred.return (Error (Unknown (c, code)))
      end
    | (500 | 503) ->
      (* 500, InternalError | 503, SlowDown | 503, ServiceUnavailable -> Throttle *)
      Deferred.return (Error Throttled)
    | code ->
      let resp = Error_response.of_xmlm_exn (xmlm_of_string body) in
      Deferred.return (Error (Unknown (code, resp.code)))

  let put_common ?credentials ?connect_timeout_ms ~endpoint ?content_type ?content_encoding ?acl ?cache_control ?expect ?(meta_headers=[]) ~bucket ~key ~body () =
    let path = sprintf "/%s/%s" bucket key in
    let headers =
      (
        [ "Content-Type", content_type;
          "Content-Encoding", content_encoding;
          "Cache-Control", cache_control;
          "x-amz-acl", acl;
        ]
        |> List.filter ~f:(function (_, Some _) -> true | (_, None) -> false)
        |> List.map ~f:(function (k, Some v) -> (k, v) | (_, None) -> failwith "Impossible")
      ) @ (meta_headers |> List.map ~f:(fun (k, v) -> (Printf.sprintf "x-amz-meta-%s" k, v)))
    in
    let sink = Body.null () in
    let cmd () =
      Aws.make_request ~endpoint ?expect ?credentials ?connect_timeout_ms ~headers ~meth:`PUT ~path ~sink ~body ~query:[] ()
    in

    do_command ~endpoint cmd >>=? fun headers ->
    let etag =
      match Headers.find_opt "etag" headers with
      | None -> failwith "Put reply did not contain an etag header"
      | Some etag -> unquote etag
    in
    Deferred.return (Ok etag)

  (**/**)

  module Stream = struct

    let get ?credentials ?connect_timeout_ms ~endpoint ?range ~bucket ~key ~data () =
      let headers =
        let r_opt = function
          | Some r -> (string_of_int r)
          | None -> ""
        in
        match range with
        | Some { first = None; last = None } -> []
        | Some { first = Some first; last } ->
          [ "Range", sprintf "bytes=%d-%s" first (r_opt last) ]
        | Some { first = None; last = Some last } when last < 0 ->
          [ "Range", sprintf "bytes=-%d" last ]
        | Some { first = None; last = Some last } ->
          [ "Range", sprintf "bytes=0-%d" last ]
        | None -> []
      in
      let path = sprintf "/%s/%s" bucket key in
      let cmd () =
        Aws.make_request ~endpoint ?credentials ?connect_timeout_ms ~sink:data ~headers ~meth:`GET ~path ~query:[] ()
      in
      do_command ~endpoint cmd >>=? fun (_headers) ->
      Deferred.return (Ok ())

    let put ?credentials ?connect_timeout_ms ~endpoint ?content_type ?content_encoding ?acl ?cache_control ?expect ?meta_headers ~bucket ~key ~data ~chunk_size ~length () =
      let body = Body.Chunked { length; chunk_size; pipe=data } in
      put_common ~endpoint ?credentials ?connect_timeout_ms ?content_type ?content_encoding ?acl ?cache_control ?expect ?meta_headers ~bucket ~key ~body ()
  end
  (* End streaming module *)

  let put ?credentials ?connect_timeout_ms ~endpoint ?content_type ?content_encoding ?acl ?cache_control ?expect ?meta_headers ~bucket ~key ~data () =
    let body = Body.String data in
    put_common ?credentials ?connect_timeout_ms ?content_type ?content_encoding ?acl ?cache_control ?expect ?meta_headers ~endpoint ~bucket ~key ~body ()

  let get ?credentials ?connect_timeout_ms ~endpoint ?range ~bucket ~key () =
    let body, data = string_sink () in
    Stream.get ?credentials ?connect_timeout_ms ?range ~endpoint ~bucket ~key ~data () >>=? fun () ->
    body >>= fun body ->
    Deferred.return (Ok body)

  let delete ?credentials ?connect_timeout_ms ~endpoint ~bucket ~key () =
    let path = sprintf "/%s/%s" bucket key in
    let sink = Body.null () in
    let cmd () =
      Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`DELETE ~path ~query:[] ~sink ()
    in
    do_command ~endpoint cmd >>=? fun _headers ->
    Deferred.return (Ok ())


  let head ?credentials ?connect_timeout_ms ~endpoint ~bucket ~key () =
    let path = sprintf "/%s/%s" bucket key in
    let sink = Body.null () in
    let cmd () =
      Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`HEAD ~path ~query:[] ~sink ()
    in
    do_command ~endpoint cmd >>=? fun headers ->
    let result =
      let (>>=) a f = match a with
        | Some x -> f x
        | None -> None
      in
      Headers.find_opt "content-length" headers >>= fun size ->
      Headers.find_opt "etag" headers >>= fun etag ->
      Headers.find_opt "last-modified" headers >>= fun last_modified ->
      let meta_headers = Headers.find_prefix ~prefix:"x-amz-meta-" headers in
      let last_modified = Time.parse_rcf1123_string last_modified in
      let size = size |> int_of_string in
      let storage_class =
        Headers.find_opt "x-amz-storage-class" headers
        |> Option.value_map ~default:Standard ~f:(fun s ->
            storage_class_of_xmlm_exn (make_xmlm_node "p" [] [`Data s])
        )
      in
      Some { storage_class; size; last_modified; key; etag = unquote etag; meta_headers = Some meta_headers}
    in
    match result with
    | Some r -> Deferred.return (Ok r)
    | None -> Deferred.return (Error (Unknown (1, "Result did not return correct headers")))

  let delete_multi ?credentials ?connect_timeout_ms ~endpoint ~bucket ~objects () =
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
        |> fun req -> Ezxmlm.to_string [ req ]
      in
      let headers = [ "Content-MD5", Base64.encode_string (Caml.Digest.string request) ] in
      let body, sink = string_sink () in
      let cmd () =
        Aws.make_request ~endpoint
          ~body:(Body.String request) ?credentials ?connect_timeout_ms ~headers
          ~meth:`POST ~query:["delete", ""] ~path:("/" ^ bucket) ~sink ()
      in
      do_command ~endpoint cmd >>=? fun _headers ->
      body >>= fun body ->
      let result = Delete_multi.result_of_xmlm_exn (xmlm_of_string body) in
      Deferred.return (Ok result)

  (** List contents of bucket in s3. *)
  let rec ls ?credentials ?connect_timeout_ms ~endpoint ?start_after ?continuation_token ?prefix ?max_keys ~bucket () =
    let max_keys = match max_keys with
      | Some n when n > 1000 -> None
      | n -> n
    in
    let query = [ Some ("list-type", "2");
                  Option.map ~f:(fun ct -> ("continuation-token", ct)) continuation_token;
                  Option.map ~f:(fun prefix -> ("prefix", prefix)) prefix;
                  Option.map ~f:(fun max_keys -> ("max-keys", string_of_int max_keys)) max_keys;
                  Option.map ~f:(fun start_after -> ("start-after", start_after)) start_after;
                ] |> filter_map ~f:(fun x -> x)
    in
    let body, sink = string_sink () in
    let cmd () =
      Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`GET ~path:("/" ^ bucket) ~query ~sink ()
    in
    do_command ~endpoint cmd >>=? fun _headers ->
    body >>= fun body ->
    let result = Ls.result_of_xmlm_exn (xmlm_of_string body) in
    let continuation = match Ls.(result.next_continuation_token) with
      | Some ct ->
        Ls.More (ls ?credentials ?connect_timeout_ms ?start_after:None ~continuation_token:ct ?prefix ~endpoint ~bucket)
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
    let init ?credentials ?connect_timeout_ms ~endpoint ?content_type ?content_encoding ?acl ?cache_control ~bucket ~key  () =
      let path = sprintf "/%s/%s" bucket key in
      let query = ["uploads", ""] in
      let headers =
        let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
        let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
        let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
        filter_map ~f:(fun x -> x) [ content_type; content_encoding; cache_control; acl ]
      in
      let body, sink = string_sink () in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers ~meth:`POST ~path ~query ~sink ()
      in
      do_command ~endpoint cmd >>=? fun _headers ->
      body >>= fun body ->
      let resp = Multipart.Initiate.of_xmlm_exn (xmlm_of_string body) in
      Ok { id = resp.Multipart.Initiate.upload_id;
           parts = [];
           bucket;
           key;
         }
      |> Deferred.return

    (** Upload a part of the file.
        Parts must be at least 5Mb except for the last part
        [part_number] specifies the part numer. Parts will be assembled in order, but
        does not have to be consecutive
    *)
    let upload_part ?credentials ?connect_timeout_ms ~endpoint t ~part_number ?expect ~data () =
      let path = sprintf "/%s/%s" t.bucket t.key in
      let query =
        [ "partNumber", string_of_int part_number;
          "uploadId", t.id ]
      in
      let sink = Body.null () in
      let cmd () =
        Aws.make_request ?expect ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`PUT ~path
          ~body:(Body.String data) ~query ~sink ()
      in
      do_command ~endpoint cmd >>=? fun headers ->
      let etag =
        Headers.find_opt "etag" headers
        |> (fun etag -> Option.value_exn ~message:"Put reply did not conatin an etag header" etag)
        |> fun etag -> unquote etag
      in
      t.parts <- { etag; part_number } :: t.parts;
      Deferred.return (Ok ())

    (** Specify a part to be a file on s3.
        [range] can be used to only include a part of the s3 file
    *)
    let copy_part ?credentials ?connect_timeout_ms ~endpoint t ~part_number ?range ~bucket ~key () =
      let path = sprintf "/%s/%s" t.bucket t.key in
      let query =
        [ "partNumber", string_of_int part_number;
          "uploadId", t.id ]
      in
      let headers =
        ("x-amz-copy-source", sprintf "/%s/%s" bucket key) ::
        Option.value_map ~default:[] ~f:(fun (first, last) ->
            [ "x-amz-copy-source-range", sprintf "bytes=%d-%d" first last ]) range
      in
      let body, sink = string_sink () in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers ~meth:`PUT ~path ~query ~sink ()
      in

      do_command ~endpoint cmd >>=? fun _headers ->
      body >>= fun body ->
      let xml = xmlm_of_string body in
      match Multipart.Copy.of_xmlm_exn xml with
      | { Multipart.Copy.etag; _ } ->
        t.parts <- { etag; part_number } :: t.parts;
        Deferred.return (Ok ())

    (** Complete the multipart upload.
        The returned etag is a opaque identifier (not md5)
    *)
    let complete ?credentials ?connect_timeout_ms ~endpoint t () =
      let path = sprintf "/%s/%s" t.bucket t.key in
      let query = [ "uploadId", t.id ] in
      let request =
        (* TODO: Sort the parts by partNumber *)
        let parts = Caml.List.sort (fun a b -> compare a.Multipart.part_number b.part_number) t.parts in
        Multipart.Complete.(xml_of_request { parts })
        |> (fun node -> Format.asprintf "%a" Ezxmlm.pp [node])
      in
      let body, sink = string_sink () in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`POST ~path ~query ~body:(Body.String request) ~sink ()
      in
      do_command ~endpoint cmd >>=? fun _headers ->
      body >>= fun body ->
      let xml = xmlm_of_string body in
      match Multipart.Complete.response_of_xmlm_exn xml with
      | { location=_; etag; bucket; key } when bucket = t.bucket && key = t.key ->
        Ok etag |> Deferred.return
      | _ ->
        Error (Unknown ((-1), "Bucket/key does not match"))
        |> Deferred.return


    (** Abort a multipart upload, deleting all specified parts *)
    let abort ?credentials ?connect_timeout_ms ~endpoint t () =
      let path = sprintf "/%s/%s" t.bucket t.key in
      let query = [ "uploadId", t.id ] in
      let sink = Body.null () in
      let cmd () =
        Aws.make_request ?credentials ~endpoint ?connect_timeout_ms ~headers:[] ~meth:`DELETE ~path ~query ~sink ()
      in
      do_command ~endpoint cmd >>=? fun _headers ->
      Deferred.return (Ok ())

    module Stream = struct
      let upload_part ?credentials ?connect_timeout_ms ~endpoint t ~part_number ?expect ~data ~length ~chunk_size () =
        let path = sprintf "/%s/%s" t.bucket t.key in
        let query =
          [ "partNumber", string_of_int part_number;
            "uploadId", t.id ]
        in
        let body = Body.Chunked { length; chunk_size; pipe=data } in
        let sink = Body.null () in
        let cmd () =
          Aws.make_request ?expect ?credentials ?connect_timeout_ms ~endpoint ~headers:[] ~meth:`PUT ~path
            ~body ~query ~sink ()
        in

        do_command ~endpoint cmd >>=? fun headers ->
        let etag =
          Headers.find_opt "etag" headers
          |> (fun etag -> Option.value_exn ~message:"Put reply did not conatin an etag header" etag)
          |> fun etag -> String.sub ~pos:1 ~len:(String.length etag - 2) etag
        in
        t.parts <- { etag; part_number } :: t.parts;
        Deferred.return (Ok ())
    end

  end

  let retry ~endpoint ~retries ~f () =
    let delay n =
      let jitter = Random.float 0.5 +. 0.5 in
      let backoff = 2.0 ** (float n) in
      (min 60.0 backoff) *. jitter
      in
    let rec inner ~endpoint ~retry_count ~redirected () =
      f ~endpoint () >>= function
      | Error (Redirect _) as e when redirected ->
        Deferred.return e
      | Error (Redirect endpoint) ->
        inner ~endpoint ~retry_count ~redirected:true ()
      | Error _ as e when retry_count = retries ->
        Deferred.return e
      | Error (Throttled) ->
        Deferred.after (delay (retry_count + 1)) >>= fun () ->
        inner ~endpoint ~retry_count:(retry_count + 1) ~redirected ()
      | Error _ ->
        inner ~endpoint ~retry_count:(retry_count + 1) ~redirected ()
      | Ok r -> Deferred.return (Ok r)
    in
    inner ~endpoint ~retry_count:0 ~redirected:false ()
end

let%test _ =
  let module Protocol = Protocol(struct type 'a result = 'a end) in
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
          <ETag>"7538d2bd85ea5dfb689ed65a0f60a7aa"</ETag>
          <Size>20</Size>
        </Contents>
        <Contents>
          <StorageClass>STANDARD</StorageClass>
          <Key>test</Key>
          <LastModified>2018-02-27T13:39:35.000Z</LastModified>
          <ETag>"7538d2bd85ea5dfb689ed65a0f60a7aa"</ETag>
          <Size>20</Size>
        </Contents>
      </ListBucketResult>
      |}
    in
    let xml = xmlm_of_string data in
    let result = Protocol.Ls.result_of_xmlm_exn xml in
    2 = (List.length result.Protocol.Ls.contents) &&
    "7538d2bd85ea5dfb689ed65a0f60a7aa" = (List.hd result.Protocol.Ls.contents).Protocol.etag

let%test "parse Error_response.t" =
  let module Protocol = Protocol(struct type 'a result = 'a end) in
  let data =
    {| <Error>
         <Code>PermanentRedirect</Code>
         <Message>The bucket you are attempting to access must be addressed using the specified endpoint. Please send all future requests to this endpoint.</Message>
         <Bucket>testbucket</Bucket>
         <Endpoint>testbucket.s3.amazonaws.com</Endpoint>
         <RequestId>9E23E3919C24476C</RequestId>
         <HostId>zdQmjTUli+pR+gwwhfGt2/s7VVerHquAPqgi9KpZ9OVsYhfF+9uAkkRJtxPcLCJKk2ZjzV1VV=</HostId>
       </Error>
    |}
  in
  let xml = xmlm_of_string data in
  let error = Protocol.Error_response.of_xmlm_exn xml in
  "PermanentRedirect" = error.Protocol.Error_response.code

let%test "parse Delete_multi.result" =
  let module Protocol = Protocol(struct type 'a result = 'a end) in
  let data =
    {| <DeleteResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
         <Error>
           <Key>test key1</Key>
           <Code>InternalError</Code>
           <Message>We encountered an internal error. Please try again.</Message>
         </Error>
         <Error>
           <Key>test key2</Key>
           <Code>InternalError</Code>
           <Message>We encountered an internal error. Please try again.</Message>
         </Error>
       </DeleteResult>
    |}
  in
  let xml = xmlm_of_string data in
  let error = Protocol.Delete_multi.result_of_xmlm_exn xml in
  2 = (List.length error.Protocol.Delete_multi.error) &&
  "InternalError" = (List.hd error.Protocol.Delete_multi.error).Protocol.Delete_multi.code
