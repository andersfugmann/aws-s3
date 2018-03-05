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


  type cmd =
    | S3toLocal of objekt * string
    | LocaltoS3 of string * objekt

  let rec retry ~delay ~retries ~(f : (unit -> 'a Deferred.Or_error.t)) () : 'a Deferred.Or_error.t =
    f () >>= function
    | Result.Error e when retries > 0 ->
      Caml.Printf.eprintf "Error: %s\n%!" (Core.Error.to_string_hum e);
      after delay >>= fun () -> retry ~delay ~retries:(retries - 1) ~f ()
    | r -> return r

  let determine_paths src dst =
    let src = Uri.of_string src in
    let dst = Uri.of_string dst in
    let is_s3 u = Uri.scheme u = Some "s3" in
    match is_s3 src, is_s3 dst with
    | (true, false) -> S3toLocal (objekt_of_uri src, Uri.path dst)
    | (false, true) -> LocaltoS3 (Uri.path src, objekt_of_uri dst)
    | (false, false) -> failwith "Use cp(1)"
    | (true, true) -> failwith "Does not support copying from s3 to s3"

  let cp profile ?first ?last src dst =
    let range = { S3.first; last } in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    (* nb client does not support preflight 100 *)
    match determine_paths src dst with
    | S3toLocal (src, dst) ->
        S3.get ~credentials ~range ~bucket:src.bucket ~key:src.key () >>=? fun data ->
        save_file dst data;
        Deferred.Or_error.return ()
    | LocaltoS3 (src, dst) ->
      let data = read_file ?first ?last src in
      S3.put ~credentials ~bucket:dst.bucket ~key:dst.key data >>=? fun _etag ->
      Deferred.Or_error.return ()

  let rm profile bucket paths =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    match paths with
    | [ key ] ->
        S3.delete ~credentials ~bucket ~key ()
    | keys ->
      let objects : S3.Delete_multi.objekt list = List.map ~f:(fun key -> { S3.Delete_multi.key; version_id = None }) keys in
      S3.delete_multi ~credentials ~bucket objects () >>=? fun _deleted ->
      Deferred.Or_error.return ()

  let ls profile ratelimit bucket prefix =
    let ratelimit_f = match ratelimit with
      | None -> fun () -> Deferred.Or_error.return ()
      | Some n -> fun () -> after (1000. /. float n) >>= Deferred.Or_error.return
    in
    let rec ls_all (result, cont) =
      Core.List.iter ~f:(fun { last_modified;  S3.Ls.key; size; etag; _ } -> Caml.Printf.eprintf "%s\t%d\t%s\t%s\n" (Time.to_string last_modified) size key (Caml.Digest.to_hex etag)) result;

      match cont with
      | S3.Ls.More continuation -> ratelimit_f ()
        >>=? retry ~retries:5 ~delay:1.0 ~f:continuation
        >>=? ls_all
      | S3.Ls.Done -> Deferred.Or_error.return ()
    in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    (* nb client does not support redirects or preflight 100 *)
    ls_all ([], S3.Ls.More (fun () -> S3.ls ~credentials ?prefix ~bucket ()))

  let exec ({ Cli.profile }, cmd) =
    begin
      match cmd with
      | Cli.Cp { src; dest; first; last } ->
        cp profile ?first ?last src dest
      | Rm { bucket; paths }->
        rm profile bucket paths
      | Ls { ratelimit; bucket; prefix } ->
        ls profile ratelimit bucket prefix
    end >>= function
    | Ok _ -> return 0
    | Error e ->
      Printf.eprintf "%s\n%!" (Error.to_string_hum e);
      return 1
end
