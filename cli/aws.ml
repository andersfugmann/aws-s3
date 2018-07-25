open StdLabels
let sprintf = Printf.sprintf
let ok_exn = function
  | Ok v -> v
  | Error exn -> raise exn

module Option = struct
  let value ~default = function
    | Some v -> v
    | None -> default

  let value_exn ~message = function
    | Some v -> v
    | None -> failwith message
end

let retries = 0

module Make(Io : Aws_s3.Types.Io) = struct
  module S3 = Aws_s3.S3.Make(Io)
  module Credentials = Aws_s3.Credentials.Make(Io)
  module Body = Aws_s3.Body.Make(Io)
  open Io
  open Deferred


  let read_file ?first ?last file =
    let data, len =
      let ic = open_in file in
      let len = in_channel_length ic in
      let data = really_input_string ic len in
      close_in ic;
      data, len
    in
    match (first, last) with
    | None, None -> data
    | first, last ->
      let first = Option.value ~default:0 first in
      let last = Option.value ~default:(len - 1) last in
      String.sub ~pos:first ~len:(last - first + 1) data

  let save_file file contents =
    let oc = open_out file in
    output_string oc contents;
    close_out oc

  type objekt = { bucket: string; key: string }
  let objekt_of_uri u =
    match String.split_on_char ~sep:'/' u with
    | ["s3:"; ""; bucket; key] ->
      { bucket; key }
    | _ -> failwith ("Illegal uri: " ^ u)

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
    let is_s3 u =
      String.split_on_char ~sep:'/' u
      |> List.hd
      |> fun scheme -> scheme = "s3:"
    in
    match is_s3 src, is_s3 dst with
    | (true, false) -> S3toLocal (objekt_of_uri src, dst)
    | (false, true) -> LocaltoS3 (src, objekt_of_uri dst)
    | (true, true) -> S3toS3 (objekt_of_uri src, objekt_of_uri dst)
    | (false, false) -> failwith "Use cp(1)"

  let rec upload_parts t scheme ~credentials ?(offset=0) ?(part_number=1) ?chunk_size data =
    let f part ?region =
      match chunk_size with
      | None ->
        S3.Multipart_upload.upload_part ~scheme ?region ~credentials t ~part_number ~data:part
      | Some chunk_size ->
        let data = Io.Pipe.create_reader ~f:(fun writer -> Io.Pipe.write writer part) in
        S3.Multipart_upload.Stream.upload_part ~scheme ?region ~credentials t ~part_number ~data ~chunk_size ~length:(String.length part)
    in
    match (String.length data - offset) with
    | 0 -> []
    | len ->
      let size = min len (5*1024*1024) in
      let part = String.sub ~pos:offset ~len:size data in
      (S3.retry ~retries ~f:(f part)) () ::
       upload_parts t scheme ~credentials ~offset:(offset + size) ~part_number:(part_number + 1) ?chunk_size data

  let cp profile scheme ?(use_multi=false) ?first ?last ?chunk_size src dst =
    let range = { S3.first; last } in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = ok_exn credentials in
    match determine_paths src dst with
    | S3toLocal (src, dst) ->
      let f ?region () = match chunk_size with
        | None ->
          S3.get ~scheme ?region ~credentials ~range ~bucket:src.bucket ~key:src.key ()
        | Some _ ->
          S3.Stream.get ~scheme ?region ~credentials ~range ~bucket:src.bucket ~key:src.key () >>=? fun body ->
          Body.to_string body >>= fun body ->
          Deferred.return (Ok body)
      in
      S3.retry ~retries ~f () >>=? fun data ->
      save_file dst data;
      Deferred.return (Ok ())
    | LocaltoS3 (src, dst) when use_multi ->
      let data = read_file ?first ?last src in
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.init ~scheme ?region ~credentials ~bucket:dst.bucket ~key:dst.key ()) () >>=? fun t ->
      let uploads = upload_parts t scheme ?chunk_size ~credentials data in
      List.fold_left ~init:(Deferred.return (Ok ())) ~f:(fun acc x -> acc >>=? fun () -> x) uploads >>=?
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.complete ?region ~credentials t ()) >>=? fun _md5 ->
      Deferred.return (Ok ())
    | LocaltoS3 (src, dst) ->
      let data = read_file ?first ?last src in
      let f = match chunk_size with
        | None ->
          fun ?region () -> S3.put ~scheme ?region ~credentials ~bucket:dst.bucket ~key:dst.key
              ~data ()
        | Some chunk_size ->
          let reader =
            Io.Pipe.create_reader ~f:(fun writer -> Io.Pipe.write writer data)
          in
          fun ?region () -> S3.Stream.put ~scheme ?region ~credentials ~bucket:dst.bucket ~key:dst.key
              ~data:reader ~chunk_size ~length:(String.length data) ()
      in
      S3.retry ~retries ~f () >>=? fun _etag ->
      Deferred.return (Ok ())
    | S3toS3 (src, dst) ->
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.init ~scheme ?region ~credentials ~bucket:dst.bucket ~key:dst.key ()) () >>=? fun t ->
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.copy_part ~scheme ?region ~credentials t ~bucket:src.bucket ~key:src.key ~part_number:1 ()) () >>=?
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.complete ~scheme ?region ~credentials t ()) >>=? fun _md5 ->
      Deferred.return (Ok ())

  let rm profile scheme bucket paths =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = ok_exn credentials in
    match paths with
    | [ key ] ->
        S3.retry ~retries ~f:(S3.delete ~scheme ~credentials ~bucket ~key) ()
    | keys ->
      let objects : S3.Delete_multi.objekt list = List.map ~f:(fun key -> { S3.Delete_multi.key; version_id = None }) keys in
      S3.retry ~retries
        ~f:(S3.delete_multi ~scheme ~credentials ~bucket ~objects) () >>=? fun _deleted ->
      Deferred.return (Ok ())

  let head profile scheme path =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = ok_exn credentials in
    let { bucket; key } = objekt_of_uri path in
    S3.head ~scheme ~credentials ~bucket ~key () >>= function
    | Ok { S3.key; etag; size; _ } ->
      Printf.printf "Key: %s, Size: %d, etag: %s\n"
        key size etag;
      Deferred.return (Ok ())
    | Error _ as e -> Deferred.return e

  let ls profile scheme ratelimit bucket prefix =
    let ratelimit_f = match ratelimit with
      | None -> fun () -> Deferred.return (Ok ())
      | Some n -> fun () -> after (1000. /. float n) >>= fun () -> Deferred.return (Ok ())
    in
    let string_of_time time =
      Ptime.of_float_s time |> function
      | None -> "<Error>"
      | Some t -> Ptime.to_rfc3339 ~space:false t
    in
    let rec ls_all (result, cont) =
      List.iter ~f:(fun { S3.last_modified; key; size; etag; _ } -> Caml.Printf.printf "%s\t%d\t%s\t%s\n%!" (string_of_time last_modified) size key etag) result;

      match cont with
      | S3.Ls.More continuation -> ratelimit_f ()
        >>=? S3.retry ~retries ~f:(fun ?region:_ () -> continuation ())
        >>=? ls_all
      | S3.Ls.Done -> Deferred.return (Ok ())
    in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = ok_exn credentials in
    S3.retry ~retries ~f:(S3.ls ~scheme ?continuation_token:None ~credentials ?prefix ~bucket) () >>=? ls_all

  let exec ({ Cli.profile; https }, cmd) =
    let scheme = match https with
      | false -> `Http
      | true -> `Https
    in
    begin
      match cmd with
      | Cli.Cp { src; dest; first; last; multi; chunk_size } ->
        cp profile scheme ~use_multi:multi ?first ?last ?chunk_size src dest
      | Rm { bucket; paths } ->
        rm profile scheme bucket paths
      | Ls { ratelimit; bucket; prefix } ->
        ls profile scheme ratelimit bucket prefix
      | Head { path } ->
        head profile scheme path
    end >>= function
    | Ok _ -> return 0
    | Error e ->
      Printf.eprintf "Error: %s\n%!" (string_of_error e);
      return 1
end
