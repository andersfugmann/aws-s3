(* Auth based on aws papers
   https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
*)
open StdLabels
let sprintf = Printf.sprintf

let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt

let hash_sha256 s =
  Digestif.SHA256.digest_string s

let hmac_sha256 ~key v =
  Digestif.SHA256.hmac_string ~key v

let to_raw sha256 = Digestif.SHA256.to_raw_string sha256

let to_hex str = Digestif.SHA256.to_hex str

let make_signing_key =
  let cache = Hashtbl.create 0 in
  fun ?(bypass_cache=false) ~date ~region ~credentials ~service () ->
    match Hashtbl.find_opt cache (credentials.Credentials.access_key, region) with
    | Some (d, signing_key) when d = date && not bypass_cache -> signing_key
    | Some _ | None ->
      let date_key = hmac_sha256 ~key:("AWS4" ^ credentials.Credentials.secret_key) date in
      let date_region_key = hmac_sha256 ~key:(to_raw date_key) region in
      let date_region_service_key = hmac_sha256 ~key:(to_raw date_region_key) service in
      let signing_key = hmac_sha256 ~key:(to_raw date_region_service_key) "aws4_request" in
      Hashtbl.replace cache (credentials.Credentials.access_key, region) (date, signing_key);
      signing_key

let make_scope ~date ~region ~service =
  sprintf "%s/%s/%s/aws4_request" date region service

let string_to_sign ~date ~time ~verb ~path ~query ~headers ~payload_sha ~scope =
  let query = List.sort ~cmp:(fun a b -> String.compare (fst a) (fst b)) query in
  assert (Headers.cardinal headers > 0);
  (* Count sizes of headers *)
  let (key_size, value_size) =
    Headers.fold (
      fun key data (h, v) -> (h + String.length key, v + String.length data)
    ) headers (0,0)
  in
  let header_count = Headers.cardinal headers in
  let canonical_headers = Buffer.create (key_size + value_size + (2 (*:\n*) * header_count)) in
  let signed_headers = Buffer.create (key_size + (Headers.cardinal headers - 1)) in

  let first = ref true in
  Headers.iter (fun key data ->
      let lower_header = String.lowercase_ascii key in
      if (not !first) then Buffer.add_string signed_headers ";";
      Buffer.add_string signed_headers lower_header;
      Buffer.add_string canonical_headers lower_header;
      Buffer.add_string canonical_headers ":";
      Buffer.add_string canonical_headers data;
      Buffer.add_string canonical_headers "\n";
      first := false;
    ) headers;

  (* Strip the trailing from signed_headers *)
  let signed_headers = Buffer.contents signed_headers in
  let canonical_query =
    query
    |> List.map ~f:(fun (k, v) -> sprintf "%s=%s" (Uri.pct_encode ~component:`Userinfo k) (Uri.pct_encode ~component:`Userinfo v))
    |> String.concat ~sep:"&"
  in


  let canonical_request = sprintf "%s\n%s\n%s\n%s\n%s\n%s"
      verb
      (Util.encode_string path)
      canonical_query
      (Buffer.contents canonical_headers)
      signed_headers
      payload_sha
  in
  log "Canonical request:\n%s\n" canonical_request;
  (* This could be cached. Its more or less static *)
  let string_to_sign = sprintf "AWS4-HMAC-SHA256\n%sT%sZ\n%s\n%s"
      date time
      scope
      (hash_sha256 canonical_request |> to_hex)
  in
  log "String to sign:\n%s\n" string_to_sign;
  log "Signed headers:\n%s\n" signed_headers;

  (string_to_sign, signed_headers)

let make_signature ~date ~time ~verb ~path
    ~headers ~query ~scope ~(signing_key:Digestif.SHA256.t) ~payload_sha =
  let (string_to_sign, signed_headers) =
    string_to_sign ~date ~time ~verb ~path ~query ~headers ~payload_sha ~scope
  in
  (hmac_sha256 ~key:(to_raw signing_key) string_to_sign |> to_hex, signed_headers)

let make_auth_header ~credentials ~scope ~signed_headers ~signature =
  sprintf "AWS4-HMAC-SHA256 Credential=%s/%s,SignedHeaders=%s,Signature=%s"
    credentials.Credentials.access_key
    scope
    signed_headers
    signature

let make_presigned_url ?(scheme=`Https) ?host ?port ~credentials ~date ~region ~path ~bucket ~verb ~duration () =
  let service = "s3" in
  let ((y, m, d), ((h, mi, s), _)) = Ptime.to_date_time date in
  let verb = match verb with
    | `Get -> "GET"
    | `Put -> "PUT" in
  let scheme = match scheme with
    | `Http -> "http"
    | `Https -> "https" in
  let date = sprintf "%02d%02d%02d" y m d in
  let time = sprintf "%02d%02d%02d" h mi s in
  let (host, path) =
    match host with
    | None -> (sprintf "%s.s3.amazonaws.com" bucket, path)
    | Some h -> (h, sprintf "/%s/%s" bucket path)
  in
  let host_header = match port with
    | None -> host
    | Some p -> String.concat ~sep:":" [host; string_of_int p]
  in
  let duration = string_of_int duration in
  let region = Region.to_string region in
  let headers = Headers.singleton "Host" host_header in
  let query = [
        ("X-Amz-Algorithm", "AWS4-HMAC-SHA256");
        ("X-Amz-Credential", sprintf "%s/%s/%s/s3/aws4_request" credentials.Credentials.access_key date region);
        ("X-Amz-Date", sprintf "%sT%sZ" date time);
        ("X-Amz-Expires", duration);
        ("X-Amz-SignedHeaders", "host");
      ] in
  let scope = make_scope ~date ~region ~service in
  let signing_key = make_signing_key ~date ~region ~service ~credentials () in
  let signature, _signed_headers =
    make_signature ~date ~time ~verb ~path ~headers ~query ~signing_key ~scope ~payload_sha:"UNSIGNED-PAYLOAD"
  in
  let query =
    ("X-Amz-Signature", signature) :: query
    |> List.map ~f:(fun (k, v) -> (k, [v]))
  in
  Uri.make ~scheme ~host ?port ~path ~query ()

let%test "presigned_url (aws)" =
  let credentials = Credentials.make
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in
  let date = match Ptime.of_date (2013, 5, 24) with Some x -> x | _ -> Ptime.epoch in
  let region = Region.of_string "us-east-1" in
  let path = "/test.txt" in
  let bucket = "examplebucket" in
  let verb = `Get in
  let duration = 86400 in
  let expected = "https://examplebucket.s3.amazonaws.com/test.txt?X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host" in
  let actual = make_presigned_url ~credentials ~date ~region ~path ~bucket ~verb ~duration () |> Uri.to_string in
  expected = actual

let%test "presigned_url (minio)" =
  let credentials = Credentials.make
      ~access_key:"access"
      ~secret_key:"secretsecret"
      ()
  in
  let date = match Ptime.of_date_time ((2019, 3, 6), ((23, 49, 50), 0)) with Some x -> x | _ -> assert false in
  let region = Region.Us_east_1 in
  let path = "dune" in
  let bucket = "example" in
  let verb = `Get in
  let duration = 604800 in
  let expected = "https://localhost:9000/example/dune?X-Amz-Signature=977ea9a866571ffba77fa1c0d843177bdba8cf004a2f61544cb3fade3f98d434&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=access/20190306/us-east-1/s3/aws4_request&X-Amz-Date=20190306T234950Z&X-Amz-Expires=604800&X-Amz-SignedHeaders=host" in
  let actual = make_presigned_url ~host:"localhost" ~port:9000 ~credentials ~date ~region ~path ~bucket ~verb ~duration () |> Uri.to_string in
  expected = actual

let%test "signing key" =
  let credentials = Credentials.make
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      ()
  in
  let date = "20120215" in
  let region = "us-east-1" in
  let service = "iam" in
  let signing_key =
    make_signing_key ~bypass_cache:true ~date ~region ~service ~credentials ()
    |> to_hex
  in
  let expected = "f4780e2d9f65fa895f9c67b32ce1baf0b0d8a43505a000a1a9e090d414db404d" in
  signing_key = expected

let%test "auth header" =
  let credentials = Credentials.make
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in
  let date = "20130524" in
  let region = "us-east-1" in
  let path = "/test.txt" in
  let service = "s3" in
  let headers =
      [ ("Host","examplebucket.s3.amazonaws.com");
        ("Range", "bytes=0-9");
        ("x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
        ("x-amz-date", "20130524T000000Z")
      ]
      |> List.fold_left ~f:(fun acc (key, value) -> Headers.add ~key ~value acc) ~init:Headers.empty
  in
  let verb = "GET" in
  let query = [] in
  let payload_sha = hash_sha256 "" |> to_hex in
  let scope = make_scope ~date ~region ~service in
  let signing_key = make_signing_key ~date ~region ~service ~credentials () in
  let signature, signed_headers =
    make_signature ~date ~time:"000000" ~verb ~path ~headers ~query ~signing_key ~scope ~payload_sha
  in
  let auth = make_auth_header ~credentials ~signature ~scope ~signed_headers in

  let expected =
    "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
  in
  auth = expected

let empty_sha_hex = hash_sha256 "" |> to_hex
let chunk_signature ~(signing_key: Digestif.SHA256.t)  ~date ~time ~scope ~previous_signature ~sha =
  let _initial = "STREAMING-AWS4-HMAC-SHA256-PAYLOAD" in
  let string_to_sign = sprintf "AWS4-HMAC-SHA256-PAYLOAD\n%sT%sZ\n%s\n%s\n%s\n%s"
      date time
      scope
      previous_signature
      empty_sha_hex
      (to_hex sha)
  in
  hmac_sha256 ~key:(to_raw signing_key) string_to_sign

let%test "chunk_signature" =
  let credentials = Credentials.make
      ~access_key:"AKIAIOSFODNN7EXAMPLE"
      ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in

  let previous_signature =
    "4f232c4386841ef735655705268965c44a0e4690baa4adea153f7db9fa80a0a9"
  in
  let date = "20130524" in
  let time = "000000" in
  let scope = "20130524/us-east-1/s3/aws4_request" in
  let signing_key = make_signing_key
      ~bypass_cache:true
      ~date
      ~region:"us-east-1"
      ~service:"s3"
      ~credentials
      ()
  in
  let sha = String.make 65536 'a' |> hash_sha256 in
  let signature = chunk_signature
      ~signing_key
      ~date ~time
      ~scope
      ~previous_signature
      ~sha
  in
  let expect = "ad80c730a21e5b8d04586a2213dd63b9a0e99e0e2307b0ade35a65485a288648" in
  signature |> to_hex = expect
