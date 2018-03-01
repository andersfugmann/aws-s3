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
module Protocol(P: sig type 'a or_error end) = struct
  module Ls = struct
    type time = Core.Time.t
    let time_of_xml_light t =
      Xml_light.to_string t |> Time.of_string

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
      etag: string [@key "ETag"];
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

    type t = (content list * cont) P.or_error
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

end

module Make(Compat : Types.Compat) = struct
  module Util_deferred = Util.Make(Compat)
  open Compat
  open Deferred.Infix
  include Protocol(struct type 'a or_error = 'a Deferred.Or_error.t end)

  type 'a command = ?retries:int -> ?credentials:Credentials.t -> ?region:Util.region -> 'a

  let do_command ?(retries=12) ~region cmd =
    let rec inner ?host ~region count =
      (* This should be the or_error *)
      cmd ?host ~region >>= fun (resp, body) ->
      let status = Cohttp.Response.status resp in
      match Code.code_of_status status with
      | code when 200 <= code && code < 300 ->
        let headers = Cohttp.Response.headers resp in
        Deferred.Or_error.return (headers, body)
      | (301 | 400) as c when count <= retries -> begin
          let open Error_response in
          (* Parse the error message. We need to know where to redirect to *)
          Cohttp_deferred.Body.to_string body >>= fun body ->
          match Error_response.t_of_xml_light (Xml.parse_string body) with
          | { code = "PermanentRedirect"; endpoint = Some host; _ } ->
            inner ~host ~region count
          | { code = "AuthorizationHeaderMalformed"; region = Some region; _ } ->
            let region = Util.region_of_string region in
            inner ?host ~region count
          | { code; _ } ->
            Deferred.return (Or_error.errorf "Unhanded error code: %d %s" c code)
          | exception e -> Deferred.Or_error.fail (Error.of_exn e)
        end
      | (500 | 503) when count <= retries ->
        (* Should actually extract the textual error code: 'NOT_READY' = 500 | 'THROTTLED' = 503 *)
        let delay = ((2.0 ** float count) /. 10.) in
        Deferred.after delay >>= fun () ->
        inner ?host ~region (count + 1)
      | code ->
        Cohttp_deferred.Body.to_string body >>= fun body ->
        Deferred.return (Or_error.errorf "Command resulted in error: %d. Error: %s" code body)
    in
    Deferred.Or_error.catch (fun () -> inner ~region 0)

  let put ?retries ?credentials ?(region=Util.Us_east_1) ?content_type ?(gzip=false) ?acl ?cache_control ~bucket ~key data =
    let path = sprintf "%s/%s" bucket key in
    let content_encoding, body = match gzip with
      | true -> Some "gzip", Util.gzip_data data
      | false -> None, data
    in
    let headers =
      let content_type     = Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
      let content_encoding = Option.map ~f:(fun ct -> ("Content-Encoding", ct)) content_encoding in
      let cache_control    = Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
      let acl              = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
      Core.List.filter_opt [ content_type; content_encoding; cache_control; acl ]
    in
    let cmd ?host ~region =
      Util_deferred.make_request ?credentials ?host ~region ~headers ~meth:`PUT ~path ~body ~query:[] ()
    in

    do_command ?retries ~region cmd >>=? fun (headers, _body) ->
    let etag =
      Header.get headers "etag"
      |> (fun etag -> Option.value_exn ~message:"Put reply did not conatin an etag header" etag)
      |> String.strip ~drop:(function '"' -> true | _ -> false)
      |> B64.decode
    in
    Deferred.Or_error.return etag

  let get ?retries ?credentials ?(region=Util.Us_east_1) ~bucket ~key () =
    let path = sprintf "%s/%s" bucket key in
    let cmd ?host ~region =
      Util_deferred.make_request ?credentials ?host ~region ~headers:[] ~meth:`GET ~path ~query:[] ()
    in
    do_command ?retries ~region cmd >>=? fun (_headers, body) ->
    Cohttp_deferred.Body.to_string body >>= fun body ->
    Deferred.return (Ok body)

  let delete ?retries ?credentials ?(region=Util.Us_east_1) ~bucket ~key () =
    let path = sprintf "%s/%s" bucket key in
    let cmd ?host ~region =
      Util_deferred.make_request ?credentials ?host ~region ~headers:[] ~meth:`DELETE ~path ~query:[] ()
    in
    do_command ?retries ~region cmd >>=? fun (_headers, _body) ->
    Deferred.return (Ok ())

  let delete_multi ?retries ?credentials ?(region=Util.Us_east_1) ~bucket objects () =
    let request =
      Delete_multi.({
          quiet=false;
          objects=objects;
        })
      |> Delete_multi.xml_of_request
      |> Xml.to_string
    in
    let headers = [ "Content-MD5", B64.encode (Caml.Digest.string request) ] in
    let cmd ?host ~region =
      Util_deferred.make_request
        ~body:request ?credentials ?host ~region ~headers
        ~meth:`POST ~query:["delete", ""] ~path:bucket ()
    in
    do_command ?retries ~region cmd >>=? fun (_headers, body) ->
    Deferred.catch (fun () ->
        Cohttp_deferred.Body.to_string body >>= fun body ->
        let result = Delete_multi.result_of_xml_light (Xml.parse_string body) in
        Deferred.return result
      )

  let rec ls ?retries ?credentials ?(region=Util.Us_east_1) ?continuation_token ?prefix ~bucket () =
    let query = [ Some ("list-type", "2");
                  Option.map ~f:(fun ct -> ("continuation-token", ct)) continuation_token;
                  Option.map ~f:(fun prefix -> ("prefix", prefix)) prefix;
                ] |> List.filter_opt
    in
    let cmd ?host ~region =
      Util_deferred.make_request ?credentials ~region ?host ~headers:[] ~meth:`GET ~path:bucket ~query ()
    in
    do_command ?retries ~region cmd >>=? fun (_headers, body) ->
    Deferred.catch (fun () ->
        Cohttp_deferred.Body.to_string body >>= fun body ->
        let result = Ls.result_of_xml_light (Xml.parse_string body) in
        let continuation = match Ls.(result.next_continuation_token) with
          | Some ct -> Ls.More (ls ?retries ?credentials ~region ~continuation_token:ct ?prefix ~bucket)
          | None -> Ls.Done
        in
        Deferred.return (Ls.(result.contents, continuation))
      )
end

module Test = struct
  open OUnit2

  module Protocol = Protocol(struct type 'a or_error = 'a Core.Or_error.t end)

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
