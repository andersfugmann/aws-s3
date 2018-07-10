(* Auth based on aws papers
   https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
*)
open Core
module HeaderMap = Map.Make(String.Caseless)

let hash_sha256 s =
  Digestif.SHA256.digest_string s

let hmac_sha256 ~key v =
  Digestif.SHA256.hmac_string ~key v

let to_hex str = Digestif.SHA256.to_hex str

let yyymmdd_of_time time =
  Date.of_time ~zone:Time.Zone.utc time
  |> Date.to_string_iso8601_basic

let iso8601_of_time time =
  let {Time.Span.Parts.hr; min; sec; _} =
    Time.to_ofday ~zone:Time.Zone.utc time
    |> Time.Ofday.to_parts
  in
  sprintf "%sT%.2d%.2d%.2dZ"
    (yyymmdd_of_time time) hr min sec

(** This should be caching within 24h *)
let make_signing_key ~time ~region ~secret_key ~service =
  let date = yyymmdd_of_time time in
  let date_key = hmac_sha256 ~key:("AWS4" ^ secret_key) date in
  let date_region_key = hmac_sha256 ~key:(date_key :> string) region in
  let date_region_service_key = hmac_sha256 ~key:(date_region_key :> string) service in
  let signing_key = hmac_sha256 ~key:(date_region_service_key :> string) "aws4_request" in
  signing_key

let string_to_sign ~time ~verb ~path ~query ~headers ~payload_sha ~region ~service =
  assert (HeaderMap.length headers > 0);
  (* Count sizes of headers *)
  let (key_size, value_size) =
    HeaderMap.fold ~f:(
      fun ~key ~data (h, v) -> (h + String.length key, v + String.length data)
    ) ~init:(0,0) headers
  in
  let canonical_headers = Buffer.create(key_size + value_size + ( 1 + 1 (*:\n*) ) * HeaderMap.length headers) in
  let signed_headers = Buffer.create (key_size + HeaderMap.length headers) in

  HeaderMap.iteri ~f:(fun ~key ~data ->
      let lower_header = String.lowercase key in
      Buffer.add_string signed_headers lower_header;
      Buffer.add_string signed_headers ";";
      Buffer.add_string canonical_headers lower_header;
      Buffer.add_string canonical_headers ":";
      Buffer.add_string canonical_headers data;
      Buffer.add_string canonical_headers "\n";
    ) headers;

  (* Strip the trailing from signed_headers *)
  Buffer.truncate signed_headers (Buffer.length signed_headers - 1);
  let signed_headers = Buffer.contents signed_headers in

  let canonical_request = sprintf "%s\n%s\n%s\n%s\n%s\n%s"
      verb
      path
      query
      (Buffer.contents canonical_headers)
      signed_headers
      payload_sha
  in
  let date = yyymmdd_of_time time in
  (** This could be cached. Its more or less static *)
  let string_to_sign = sprintf "AWS4-HMAC-SHA256\n%s\n%s/%s/%s/aws4_request\n%s"
      (iso8601_of_time time)
      date region service
      (hash_sha256 canonical_request |> to_hex)
  in
  (string_to_sign, signed_headers)

let make_authorization ~time ~verb ~credentials ~path ~headers ~query ~region ~service ~payload_sha =
  let signing_key = make_signing_key ~time ~region ~service ~secret_key:credentials.Credentials.secret_key in
  let (string_to_sign, signed_headers) =
    string_to_sign ~region ~time ~verb ~path ~query ~headers ~payload_sha ~service
  in
  let signature = hmac_sha256 ~key:(signing_key :> string) string_to_sign |> to_hex in
  let auth = sprintf "AWS4-HMAC-SHA256 Credential=%s/%s/%s/%s/aws4_request,SignedHeaders=%s,Signature=%s"
      credentials.Credentials.access_key
      (yyymmdd_of_time time)
      region
      service
      signed_headers
      signature
  in
  auth

let%test _ =
  let creds = Credentials.make_credentials
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      ()
  in
  let time = Time.(of_span_since_epoch (Span.of_int_sec 1329346800)) in
  let region = "us-east-1" in
  let service = "iam" in
  let signing_key =
    make_signing_key ~time ~region ~service ~secret_key:creds.Credentials.secret_key
    |> to_hex
  in
  let expected = "f4780e2d9f65fa895f9c67b32ce1baf0b0d8a43505a000a1a9e090d414db404d" in
  signing_key = expected

let%test _ =
  let credentials = Credentials.make_credentials
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in
  let time = Time.(of_span_since_epoch (Span.of_int_sec 1369353600)) in
  let region = "us-east-1" in
  let path = "/test.txt" in
  let service = "s3" in
  let headers = HeaderMap.of_alist_exn
      [ ("Host","examplebucket.s3.amazonaws.com");
        ("Range", "bytes=0-9");
        ("x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
        ("x-amz-date", "20130524T000000Z")
      ]
  in
  let verb = "GET" in
  let query = "" in
  let payload_sha = hash_sha256 "" |> to_hex in
  let auth = make_authorization
      ~time ~verb ~credentials ~path ~headers ~query ~region ~service ~payload_sha
  in

  let expected =
    "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
  in
  auth = expected
