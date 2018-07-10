(* Auth based on aws papers
   https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
*)
open Core

module HeaderMap = Map.Make(String.Caseless.compare)


let hash_sha256 (s : string) =
  Digestif.SHA256.digest_string s
  |> Digestif.SHA256.to_hex

let mac ~key v =
  Digestif.SHA256.hmac_string ~key v

let to_hex digest = Digestif.SHA256.to_hex

let yyymmdd_of_time time =
  Date.of_time ~zone:Time.Zone.utc time
  |> Date.to_string_iso8601_basic

let iso8601_of_time time =
  let {hr; min; sec; _} =
    Time.to_ofday time
    |> Time.Ofday.to_parts s
  in
  sprintf "%sT%.2d%.2d%.2dZ"
    (yyymmdd_of_time time) hr min sec

let make_signing_key ~time ~region ~secret_access_key =
  let date_key = mac ~key:("AWS4" ^ secret_access_key) date in
  let date_region_key = mac ~key:(date_key :> string) (string_of_region region) in
  let date_region_service_key = mac ~key:(date_region_key :> string) "s3" in
  let signing_key = mac ~key:(date_region_service_key :> string) "aws4_request" in
  (signing_key :> string)
in

let string_to_sign ~time ~verb ~path ~query ~headers ~payload_sha_string =
  assert (HeaderMap.length headers > 0);
  (* Count sizes of headers *)
  let (key_size, value_size) =
    HeaderMap.fold ~f:(
      fun ~key ~data (h, v) -> (h + String.length key, v + String.length v)
    ) ~init:(0,0) headers
  in
  let canonical_headers = Buffer.create(key_size + value_size + ( 1 + 1 (*:\n*) ) * HeaderMap.length headers) in
  let signed_headers = Buffer.create (key_size + HeaderMap.length headers) in

  HeaderMap.iteri ~f:(fun ~key ~data ->
      let lower_header = String.lowercase key in
      Buffer.add_string signed_headers lower_header;
      Buffer.add_string signed_headers ",";
      Buffer.add_string canonical_headers lower_header;
      Buffer.add_string canonical_headers ":";
      Buffer.add_string canonical_headers data;
      Buffer.add_string canonical_headers "\n";
    ) headers;

  (* Strip the trailing from signed_headers *)
  Buffer.truncate signed_headers (Buffer.length signed_headers - 1);

  let canonical_request = sprintf "%s\n%s\n%s\n%s\n%s\n%s"
      verb
      path
      query
      (Buffer.contents canonical_headers)
      (Buffer.contents signed_headers)
      payload_sha_string
  in


  (** This could be cached. Its more or less static *)
  let scope = sprintf "%s/%s/s3/aws4_request" date region in
  let string_to_sign = sprintf "AWS4-HMAC-SHA256\n%s\n%s/%s/s3/aws4_request"
      (iso8601_of_time time)
      (yyymmdd_of_time time) region
      (hash_sha256 canonical_request |> to_hex)
  in
  (string_to_sign, signed_headers)

type payload = Empty
             | Plain of string
             | Stream

let empty_sha
let string_of_payload = function
  | Empty ->
let make_authorization ~time ~creds ~headers ~region ~payload_sha =
  let signing_key = make_signing_key ~time ~region ~secret_access_key in
  let (string_to_sign, signed_headers) = string_to_sign ~time ~verb ~path ~query ~headers ~payload_sha_string

    (auto string_to_sign_and_signed_headers = string_to_sign(time, path, region, headers);
            std::string signature = Util::hex_of_string(Util::hmac_sha256(signing_key, string_to_sign_and_signed_headers.first));
            std::ostringstream header_value;
            header_value << "AWS4-HMAC-SHA256 "
                         << "Credential=" << creds.access_key << "/" << Util::yyyymmdd(time) << "/" << region << "/s3/aws4_request" << ","
                         << "SignedHeaders=" << string_to_sign_and_signed_headers.second << ","
                         << "Signature=" << signature;
            return header_value.str();
        }
    }
