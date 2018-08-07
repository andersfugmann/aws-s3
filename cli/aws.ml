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


module Make(Io : Aws_s3.Types.Io) = struct
  module S3 = Aws_s3.S3.Make(Io)
  module Credentials = Aws_s3.Credentials.Make(Io)
  module Body = Aws_s3.Body.Make(Io)
  open Io
  open Deferred

  let file_length file =
    let ic = open_in file in
    let len = in_channel_length ic in
    close_in ic;
    len

  let read_file ~pos ~len file =
    let ic = open_in file in
    seek_in ic pos;
    let data = really_input_string ic len in
    close_in ic;
    data

  let file_reader ?(chunk_size=4097) ~pos ~len file =
    (* Create a stream with the data *)
    let ic = open_in file in
    (* Seek first *)
    seek_in ic pos;
    let rec read len writer =
      match len with
      | 0 -> return ()
      | n when n < chunk_size ->
        let data = Pervasives.really_input_string ic n in
        Io.Pipe.write writer data
      | n ->
        let data = Pervasives.really_input_string ic chunk_size in
        Io.Pipe.write writer data >>= fun () ->
        (* Yield *)
        after 0.0 >>= fun () ->
        read (n - chunk_size) writer
    in
    let reader = Io.Pipe.create_reader ~f:(read len) in
    Io.Deferred.async (Io.Pipe.closed reader >>= fun () -> close_in ic; return ());
    reader

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

  let rec upload_parts t scheme ~retries ~expect ~credentials ?(offset=0) ~total ?(part_number=1) ?chunk_size src =
    let f ~size ?region ()=
      match chunk_size with
      | None ->
        let data = read_file ~pos:offset ~len:size src in
        S3.Multipart_upload.upload_part ~scheme ~expect ?region ~credentials t ~part_number ~data ()
      | Some chunk_size ->
        (* Create a reader for this section *)
        let reader = file_reader ~pos:offset ~len:size src in
        S3.Multipart_upload.Stream.upload_part ~scheme ~expect ?region ~credentials t ~part_number ~data:reader ~chunk_size ~length:size ()
    in
    match (total - offset) with
    | 0 -> []
    | len ->
      let size = min len (5*1024*1024) in
      (S3.retry ~retries ~f:(f ~size)) () ::
       upload_parts t scheme ~retries ~expect ~credentials ~offset:(offset + size) ~total ~part_number:(part_number + 1) ?chunk_size src

  let cp profile scheme ~retries ~expect ?(use_multi=false) ?first ?last ?chunk_size src dst =
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
      let offset = match first with
        | None -> 0
        | Some n -> n
      in
      let last = match last with
        | None -> file_length src
        | Some n -> n
      in
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.init ~scheme ?region ~credentials ~bucket:dst.bucket ~key:dst.key ()) () >>=? fun t ->
      let uploads = upload_parts t ~retries ~expect scheme ?chunk_size ~credentials ~offset ~total:last src in
      List.fold_left ~init:(Deferred.return (Ok ())) ~f:(fun acc x -> acc >>=? fun () -> x) uploads >>=?
      S3.retry ~retries
        ~f:(fun ?region () -> S3.Multipart_upload.complete ?region ~credentials t ()) >>=? fun _md5 ->
      Deferred.return (Ok ())
    | LocaltoS3 (src, dst) ->
      let pos = Option.value ~default:0 first in
      let len = match last with
        | None -> file_length src - pos
        | Some l -> l - pos
      in
      let f = match chunk_size with
        | None ->
          let data = read_file ~pos ~len src in
          fun ?region () -> S3.put ~scheme ?region ~expect ~credentials ~bucket:dst.bucket ~key:dst.key
              ~data ()
        | Some chunk_size ->
          let reader = file_reader ~pos ~len src in
          fun ?region () -> S3.Stream.put ~scheme ~expect ?region ~credentials ~bucket:dst.bucket ~key:dst.key
              ~data:reader ~chunk_size ~length:len ()
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

  let rm profile ~retries scheme bucket paths =
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

  let head profile ~retries scheme path =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = ok_exn credentials in
    let { bucket; key } = objekt_of_uri path in
    S3.retry ~retries
        ~f:(S3.head ~scheme ~credentials ~bucket ~key) () >>= function
    | Ok { S3.key; etag; size; _ } ->
      Printf.printf "Key: %s, Size: %d, etag: %s\n"
        key size etag;
      Deferred.return (Ok ())
    | Error _ as e -> Deferred.return e

  let ls profile ~retries ~max_keys scheme ratelimit bucket prefix =
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
    S3.retry ~retries ~f:(S3.ls ~scheme ?continuation_token:None ~credentials ~max_keys ?prefix ~bucket) () >>=? ls_all

  let exec ({ Cli.profile; https; retries; ipv6; expect; max_keys }, cmd) =
    let () =
      match ipv6 with
      | true -> S3.set_connection_type Unix.PF_INET6
      | false -> ()
    in
    let scheme = match https with
      | false -> `Http
      | true -> `Https
    in
    begin
      match cmd with
      | Cli.Cp { src; dest; first; last; multi; chunk_size } ->
        cp profile ~retries ~expect scheme ~use_multi:multi ?first ?last ?chunk_size src dest
      | Rm { bucket; paths } ->
        rm profile ~retries scheme bucket paths
      | Ls { ratelimit; bucket; prefix } ->
        ls profile ~retries ~max_keys scheme ratelimit bucket prefix
      | Head { path } ->
        head profile ~retries scheme path
    end >>= function
    | Ok _ -> return 0
    | Error e ->
      Printf.eprintf "Error: %s\n%!" (string_of_error e);
      return 1
end
