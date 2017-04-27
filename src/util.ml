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

type region =
  | Ap_northeast_1 (* Asia Pacific (Tokyo) *)
  | Ap_southeast_1 (* Asia Pacific (Singapore) *)
  | Ap_southeast_2 (* Asia Pacific (Sydney) *)
  | Eu_central_1   (* EU (Frankfurt) *)
  | Eu_west_1      (* EU (Ireland) *)
  | Sa_east_1      (* South America (Sao Paulo) *)
  | Us_east_1      (* US East (N. Virginia) *)
  | Us_west_1      (* US West (N. California) *)
  | Us_west_2      (* US West (Oregon) *)


let string_of_region = function
  | Ap_northeast_1 -> "ap-northeast-1"
  | Ap_southeast_1 -> "ap-southeast-1"
  | Ap_southeast_2 -> "ap-southeast-2"
  | Eu_central_1 -> "eu-central-1"
  | Eu_west_1 -> "eu-west-1"
  | Sa_east_1 -> "sa-east-1"
  | Us_east_1 -> "us-east-1"
  | Us_west_1 -> "us-west-1"
  | Us_west_2 -> "us-west-2"

let region_of_string = function
  | "ap-northeast-1" -> Ap_northeast_1
  | "ap-southeast-1" -> Ap_southeast_1
  | "ap-southeast-2" -> Ap_southeast_2
  | "eu-central-1" -> Eu_central_1
  | "eu-west-1" -> Eu_west_1
  | "sa-east-1" -> Sa_east_1
  | "us-east-1" -> Us_east_1
  | "us-west-1" -> Us_west_1
  | "us-west-2" -> Us_west_2
  | s -> failwith ("Unknown region: " ^ s)

let region_host_string = function
  | Us_east_1 -> "s3.amazonaws.com"
  | region -> string_of_region region |> sprintf "s3-%s.amazonaws.com"

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

let make_request ?body ?(region=Us_east_1) ?(credentials:Credentials.t option) ~headers ~meth ~path ~query () =
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

let bool = Str.regexp_case_fold "^\\(true\\|false\\)$"
let integer = Str.regexp "^[0-9]+$"
let float = Str.regexp "^[0-9]+\\([.][0-9]*\\)?$"

(** Convert an xml structure to yojson.
    This allows us to use ppx_deriving_yojson on xml structures
*)

let yojson_of_xml xml =
  (* Group a sorted list of elements such that all `Assocs are made into lists *)
  let rec group acc elems =
    match (acc, elems) with
    | (key, `List v) :: vs, (key', (`Assoc v')) :: xs when key = key' ->
        group ((key, `List ( (`Assoc v') :: v)) :: vs) xs
    | vs, (key, (`Assoc v)) :: xs -> group ((key, `List [`Assoc v]) :: vs) xs
    | vs, x :: xs -> group (x :: vs) xs
    | vs, [] -> vs
  in
  let yojson_of_string = function
    | b when Str.string_match bool b 0    -> `Bool (bool_of_string b)
    | i when Str.string_match integer i 0 -> `Int (int_of_string i)
    | f when Str.string_match float f 0   -> `Float (float_of_string f)
    | s -> `String s
  in
  let rec inner = function
    | Xml.PCData _s -> failwith "unsupported"
    | Xml.Element (n, _, [ ]) -> (n, `Null)
    | Xml.Element (n, _, [ Xml.PCData s ]) -> (n, yojson_of_string s)
    | Xml.Element (n, _, es) ->
        let elements =
          List.map ~f:inner es
          |> List.sort ~cmp:(fun (n, _) (m, _) -> String.compare n m)
          |> group []
        in
        (n, `Assoc elements)
  in
  inner xml

let rec xml_of_yojson ((name : string), (value : Yojson.Safe.json)) : Xml.xml =
  let v = match value with
    | `Assoc s -> List.map ~f:xml_of_yojson s
    | `List l -> List.map ~f:(fun e -> xml_of_yojson (name, e)) l
    | `String s -> [ Xml.PCData s ]
    | `Intlit s -> [ Xml.PCData s ]
    | `Int i -> [ Xml.PCData (string_of_int i) ]
    | `Float f -> [ Xml.PCData (string_of_float f) ]
    | `Bool b -> [ Xml.PCData (string_of_bool b) ]
    | `Null -> []
    | `Tuple _
    | `Variant _ -> failwith "Tuple and variant not supported"
  in
  Xml.Element (name, [], v )

let decode ~name ~f s =
  match Xml.parse_string s |> yojson_of_xml with
  | n, v when n = name -> begin
      match f v with
      | R.Ok t -> t
      | R.Error s -> failwith s
    end
  | v, _ -> failwith (sprintf "Found node '%s'. Expected '%s'" v name)


module Test = struct
  open OUnit2

  let async f ctx =
    Thread_safe.block_on_async_exn (fun () -> f ctx)

  open Async.Std

  let gunzip data =
    Process.create ~prog:"gunzip" ~args:[ "--no-name"; "-" ] () >>= fun proc ->
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
    __MODULE__ >::: [
      "gzip" >:: async test_gzip
    ]

end
