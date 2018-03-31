open Core
(* We need to make a functor over deferred *)
module Make(Compat: Aws_s3.Types.Compat) = struct
  module S3 = Aws_s3.S3.Make(Compat)
  module Credentials = Aws_s3.Credentials.Make(Compat)
  open Compat
  open Compat.Deferred
  open Compat.Deferred.Infix

  let read_file ?first ?last file =
    let data = Core.In_channel.(with_file file ~f:input_all) in
    let len = String.length data in
    match (first, last) with
    | None, None -> data
    | first, last ->
      let first = Option.value ~default:0 first in
      let last = Option.value ~default:(len - 1) last in
      String.sub ~pos:first ~len:(last - first + 1) data

  let save_file file contents =
    Core.Out_channel.(with_file file ~f:(fun c -> output_string c contents))

  type objekt = { bucket: string; key: string }
  let objekt_of_uri u = { bucket = (Option.value_exn ~message:"No Host in uri" (Uri.host u));
                          key = String.drop_prefix (Uri.path u) 1 (* Remove the beginning '/' *) }


  let string_of_error = function
    | S3.Redirect _ -> "Redirect"
    | S3.Throttled -> "Throttled"
    | S3.Unknown (code, msg) -> sprintf "Unknown: %d, %s" code msg
    | S3.Not_found -> "Not_found"

  type cmd =
    | S3toLocal of objekt * string
    | LocaltoS3 of string * objekt
    | S3toS3 of objekt * objekt

  let determine_paths src dst =
    let src = Uri.of_string src in
    let dst = Uri.of_string dst in
    let is_s3 u = Uri.scheme u = Some "s3" in
    match is_s3 src, is_s3 dst with
    | (true, false) -> S3toLocal (objekt_of_uri src, Uri.path dst)
    | (false, true) -> LocaltoS3 (Uri.path src, objekt_of_uri dst)
    | (true, true) -> S3toS3 (objekt_of_uri src, objekt_of_uri dst)
    | (false, false) -> failwith "Use cp(1)"

  let rec upload_parts t ~credentials ?(offset=0) ?(part_number=1) data =
    match (String.length data - offset) with
    | 0 -> []
    | n when n > 5*1024*1024 ->
      let len = 5*1024*1024 in
      let part = String.sub ~pos:offset ~len data in
      (S3.retry ~retries:5
         ~f:(fun ?region -> S3.Multipart_upload.upload_part ?region ~credentials t ~part_number ~data:part) ()) ::
      upload_parts t ~credentials ~offset:(offset + 5*1024*1024) ~part_number:(part_number + 1) data
    | len ->
      let part = String.sub ~pos:offset ~len data in
      [ S3.retry ~retries:5
         ~f:(fun ?region -> S3.Multipart_upload.upload_part ?region ~credentials t ~part_number ~data:part) () ]

  let cp profile ?(use_multi=false) ?first ?last src dst =
    let range = { S3.first; last } in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    match determine_paths src dst with
    | S3toLocal (src, dst) ->
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.get ?region ~credentials ~range ~bucket:src.bucket ~key:src.key ()) () >>=? fun data ->
      save_file dst data;
      Deferred.return (Ok ())
    | LocaltoS3 (src, dst) when use_multi ->
      let data = read_file ?first ?last src in
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.Multipart_upload.init ?region ~credentials ~bucket:dst.bucket ~key:dst.key ()) () >>=? fun t ->
      let uploads = upload_parts t ~credentials data in
      List.fold_left ~init:(Deferred.return (Ok ())) ~f:(fun acc x -> acc >>=? fun () -> x) uploads >>=?
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.Multipart_upload.complete ?region ~credentials t ()) >>=? fun _md5 ->
      Deferred.return (Ok ())
    | LocaltoS3 (src, dst) ->
      let data = read_file ?first ?last src in
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.put ?region ~credentials ~bucket:dst.bucket ~key:dst.key ~data ()) () >>=? fun _etag ->
      Deferred.return (Ok ())
    | S3toS3 (src, dst) ->
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.Multipart_upload.init ?region ~credentials ~bucket:dst.bucket ~key:dst.key ()) () >>=? fun t ->
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.Multipart_upload.copy_part ?region ~credentials t ~bucket:src.bucket ~key:src.key ~part_number:1 ()) () >>=?
      S3.retry ~retries:5
        ~f:(fun ?region () -> S3.Multipart_upload.complete ?region ~credentials t ()) >>=? fun _md5 ->
      Deferred.return (Ok ())

  let rm profile bucket paths =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    match paths with
    | [ key ] ->
        S3.retry ~retries:5 ~f:(S3.delete ~credentials ~bucket ~key) ()
    | keys ->
      let objects : S3.Delete_multi.objekt list = List.map ~f:(fun key -> { S3.Delete_multi.key; version_id = None }) keys in
      S3.retry ~retries:5
        ~f:(S3.delete_multi ~credentials ~bucket ~objects) () >>=? fun _deleted ->
      Deferred.return (Ok ())

  let ls profile ratelimit bucket prefix =
    let ratelimit_f = match ratelimit with
      | None -> fun () -> Deferred.return (Ok ())
      | Some n -> fun () -> after (1000. /. float n) >>= fun () -> Deferred.return (Ok ())
    in
    let rec ls_all (result, cont) =
      Core.List.iter ~f:(fun { last_modified;  S3.Ls.key; size; etag; _ } -> Caml.Printf.printf "%s\t%d\t%s\t%s\n%!" (Time.to_string last_modified) size key etag) result;

      match cont with
      | S3.Ls.More continuation -> ratelimit_f ()
        >>=? S3.retry ~retries:5 ~f:(fun ?region:_ () -> continuation ())
        >>=? ls_all
      | S3.Ls.Done -> Deferred.return (Ok ())
    in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    S3.retry ~retries:5 ~f:(S3.ls ?continuation_token:None ~credentials ?prefix ~bucket) () >>=? ls_all
  let exec ({ Cli.profile }, cmd) =
    begin
      match cmd with
      | Cli.Cp { src; dest; first; last; multi } ->
        cp profile ~use_multi:multi ?first ?last src dest
      | Rm { bucket; paths }->
        rm profile bucket paths
      | Ls { ratelimit; bucket; prefix } ->
        ls profile ratelimit bucket prefix
    end >>= function
    | Ok _ -> return 0
    | Error e ->
      Printf.eprintf "Error: %s\n%!" (string_of_error e);
      return 1
end
