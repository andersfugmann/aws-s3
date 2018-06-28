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

let ksrt = fun (k,_) (k',_) -> String.compare k k'

module Compat = struct
  (* Things we need to make this happen that, ideally, we'd like other
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
              Buffer.add_string buf "%25"
        end
      | _ -> Buffer.add_string buf (Printf.sprintf "%%%X" (Char.to_int c))
    done;
    Buffer.contents buf

  let encode_query_string uri =
    (* Sort and encode query string.
       Note that AWS wants null keys to have '=' for all keys.
       Also it seems that amazon is not following standard rules for argument url encoding, but require that
       '+' is pct encoded as '%2B'.
    *)
    Uri.query uri
    |> Caml.List.sort ksrt
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
    (* Core.Time doesn't have a format function that takes a timezone *)
    let d, s = Time.to_date_ofday ~zone:Time.Zone.utc t in
    let open Time.Span.Parts in
    let {hr; min; sec; _} = Time.Ofday.to_parts s in
    sprintf "%sT%.2d%.2d%.2dZ"
      (Date.to_string_iso8601_basic d) hr min sec
end

type region =
 | Ap_northeast_1  (* Asia Pacific (Tokyo) *)
 | Ap_northeast_2  (* Asia Pacific (Seoul) *)
 | Ap_northeast_3  (* Asia Pacific (Osaka-Local) *)
 | Ap_southeast_1  (* Asia Pacific (Singapore) *)
 | Ap_southeast_2  (* Asia Pacific (Sydney) *)
 | Ap_south_1      (* Asia Pacific (Mumbai) *)
 | Eu_central_1    (* EU (Frankfurt) *)
 | Cn_northwest_1  (* China (Ningxia)        *)
 | Cn_north_1      (* China (Beijing)        *)
 | Eu_west_1       (* EU (Ireland)   *)
 | Eu_west_2       (* EU (London)    *)
 | Eu_west_3       (* EU (Paris)     *)
 | Sa_east_1       (* South America (São Paulo)      *)
 | Us_east_1       (* US East (N. Virginia) *)
 | Us_east_2       (* US East (Ohio) *)
 | Us_west_1       (* US West (N. California) *)
 | Us_west_2       (* US West (Oregon) *)
 | Ca_central_1    (* Canada - central *)
 | Other of string (* Other unknown *)

let string_of_region = function
  | Ap_northeast_1 -> "ap-northeast-1"
  | Ap_northeast_2 -> "ap-northeast-2"
  | Ap_northeast_3 -> "ap-northeast-3"
  | Ap_southeast_1 -> "ap-southeast-1"
  | Ap_southeast_2 -> "ap-southeast-2"
  | Ap_south_1     -> "ap-south-1"
  | Eu_central_1   -> "eu-central-1"
  | Cn_northwest_1 -> "cn-northwest-1"
  | Cn_north_1     -> "cn-north-1"
  | Eu_west_1      -> "eu-west-1"
  | Eu_west_2      -> "eu-west-2"
  | Eu_west_3      -> "eu-west-3"
  | Sa_east_1      -> "sa-east-1"
  | Us_east_1      -> "us-east-1"
  | Us_east_2      -> "us-east-2"
  | Us_west_1      -> "us-west-1"
  | Us_west_2      -> "us-west-2"
  | Ca_central_1   -> "ca-central-1"
  | Other s        -> s

let region_of_string = function
  | "ap-northeast-1" -> Ap_northeast_1
  | "ap-northeast-2" -> Ap_northeast_2
  | "ap-northeast-3" -> Ap_northeast_3
  | "ap-southeast-1" -> Ap_southeast_1
  | "ap-southeast-2" -> Ap_southeast_2
  | "ap-south-1"     -> Ap_south_1
  | "eu-central-1"   -> Eu_central_1
  | "cn-northwest-1" -> Cn_northwest_1
  | "cn-north-1"     -> Cn_north_1
  | "eu-west-1"      -> Eu_west_1
  | "eu-west-2"      -> Eu_west_2
  | "eu-west-3"      -> Eu_west_3
  | "sa-east-1"      -> Sa_east_1
  | "us-east-1"      -> Us_east_1
  | "us-east-2"      -> Us_east_2
  | "us-west-1"      -> Us_west_1
  | "us-west-2"      -> Us_west_2
  | "ca-central-1"   -> Ca_central_1
  | s                -> failwith ("Unknown region: " ^ s)


let region_of_host host =
  match String.split ~on:'.' host |> List.rev with
  | "com" :: "amazonaws" :: "s3" :: _  ->
    Us_east_1
  | "com" :: "amazonaws" :: host :: _ ->
    String.chop_prefix host ~prefix:"s3-"
    |> Option.value ~default:host
    |> region_of_string
  | _ -> failwith "Cannot parse region from host"

let host_of_region region =
    string_of_region region |> sprintf "s3.%s.amazonaws.com"

type 'content body = {
  content: 'content;
  length: int;
  hash: Digestif.SHA256.Bytes.t;
}

module Auth = struct
  (** AWS S3 Authorization *)
  let digest_body (body : _ body) =
    let open Digestif.SHA256.Bytes in
    body.hash
    |> to_hex
    |> Caml.Bytes.to_string

  let digest s =
    let open Digestif.SHA256.Bytes in
    Caml.Bytes.of_string s
    |> digest
    |> to_hex
    |> Caml.Bytes.to_string

  let mac ~key v =
    let key = Caml.Bytes.of_string key in
    let v = Caml.Bytes.of_string v in
    Digestif.SHA256.Bytes.hmac ~key v
    |> Caml.Bytes.to_string

  let to_hex (k : string) =
    let k = Caml.Bytes.of_string k in
    let h = Digestif.SHA256.Bytes.to_hex k |> Caml.Bytes.to_string in
    h

  let empty_digest = digest ""
  let make_amz_headers ?credentials ?body time =
    (* Return x-amz-date and x-amz-sha256 headers *)
    let hashed_payload =
      match body with
        None -> empty_digest
      | Some s -> digest_body s
    in
    let token_header = match credentials with
      | Some { Credentials.aws_token = Some token; _ } ->
        [("x-amz-security-token", token)]
      | _ -> []
    in
    let headers =
      ("x-amz-content-sha256", hashed_payload) ::
      ("x-amz-date", Compat.format_time time) ::
      token_header
    in
    (headers, hashed_payload)

  let canonical_request hashed_payload (request : Cohttp.Request.t) =
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
                         |> Caml.List.sort ksrt
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

  let string_to_sign ?time ~scope canonical_request =
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


  (** Even hough the Aws documentation states that the signing key will last forever,
      we still use the date as cache key also *)
  let make_signing_key =
    let cache = Caml.Hashtbl.create 0 in

    let make ~date ~region ~secret_access_key =
      let date_key = mac ~key:("AWS4" ^ secret_access_key) date in
      let date_region_key = mac ~key:date_key (string_of_region region) in
      let date_region_service_key = mac ~key:date_region_key "s3" in
      let signing_key = mac ~key:date_region_service_key "aws4_request" in
      signing_key
    in

    fun ?date ~region ~secret_access_key ->
      let date = match date with
          None -> Date.today ~zone:Time.Zone.utc |> Date.to_string_iso8601_basic
        | Some d -> Date.to_string_iso8601_basic d in
      match Caml.Hashtbl.find cache (region, secret_access_key) with
      | (date', signing_key) when date' = date ->
        signing_key
      | _ ->
        let signing_key = make ~date ~region ~secret_access_key in
        Caml.Hashtbl.replace cache (region, secret_access_key) (date, signing_key);
        signing_key
      | exception _ ->
        let signing_key = make ~date ~region ~secret_access_key in
        Caml.Hashtbl.replace cache (region, secret_access_key) (date, signing_key);
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
    let signature = mac ~key:signing_key string_to_sign |> to_hex in

    let auth_header = sprintf
        "AWS4-HMAC-SHA256 Credential=%s,SignedHeaders=%s,Signature=%s"
        creds signed_headers signature

    in
    [("Authorization", auth_header);]

end

module Make(C : Types.Compat) = struct
  open C

  type nonrec body = Cohttp_deferred.Body.t body

  let make_body ~content ~length ~hash : body =
    { content; length; hash }

  let body_of_string s : body = {
    content = Cohttp_deferred.Body.of_string s;
    length = String.length s;
    hash = Digestif.SHA256.Bytes.digest (Caml.Bytes.of_string s);
  }

  let make_request ~scheme ?body ?(region=Us_east_1) ?(credentials:Credentials.t option) ~headers ~meth ~path ~query () =
    let host_str = host_of_region region in
    let uri = Uri.make
        ~host:host_str
        ~path
        ~query:(List.map ~f:(fun (k,v) -> k, [v]) query)
        ()
    in
    let time = Time.now () in
    (* If PUT|POST add content length *)
    let content_length = match meth with
      | `PUT | `POST ->
        let length =
          Option.value_map ~f:(fun (body: body) -> body.length) ~default:0 body
        in
        Some ("Content-Length", Int.to_string length)
      | _ -> None
    in
    let (amz_headers, hashed_payload) = Auth.make_amz_headers ?credentials time ?body in
    let headers =
      ("User-Agent", "aws-s3 ocaml client") :: ("Host", host_str) :: (List.filter_opt [ content_length ]) @ headers @ amz_headers
    in

    let request = Request.make ~meth:(meth :> Code.meth)
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
    | `PUT
    | `POST ->
      let body = Option.map ~f:(fun { content; _} -> content) body in
      Cohttp_deferred.Client.request ~scheme ?body request
    | `GET
    | `DELETE
    | `HEAD -> Cohttp_deferred.Client.request ~scheme request

end
