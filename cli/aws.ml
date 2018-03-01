open Core
(* We need to make a functor over deferred *)
module Make(Compat: Aws_s3.Types.Compat) = struct
  module S3 = Aws_s3.S3.Make(Compat)
  module Credentials = Aws_s3.Credentials.Make(Compat)
  open Compat
  open Compat.Deferred
  open Compat.Deferred.Infix

  let read_file file =
    Core.In_channel.(with_file file ~f:input_all)

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

  let cp profile src dst () =
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    (* nb client does not support preflight 100 *)
    match determine_paths src dst with
    | S3toLocal (src, dst) ->
      begin
        S3.get ~credentials ~bucket:src.bucket ~key:src.key () >>= function
        | Ok data ->
          save_file dst data;
          return ()
        | Error _ ->
          return ()
      end
    | LocaltoS3 (src, dst) ->
      let data = read_file src in
      S3.put ~credentials ~bucket:dst.bucket ~key:dst.key data >>= function
      | Ok _etag ->
        return ()
      | Error _ ->
        return ()

  let rm profile path () =
    let objekt = Uri.of_string path |> objekt_of_uri in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    S3.delete ~credentials ~bucket:objekt.bucket ~key:objekt.key () >>= function
    | Ok () -> return ()
    | Error _ ->
      return ()

  let ls profile ratelimit bucket prefix () =
    let ratelimit_f = match ratelimit with
      | None -> fun () -> Deferred.Or_error.return ()
      | Some n -> fun () -> after (1000. /. float n) >>= Deferred.Or_error.return
    in
    let rec ls_all (result, cont) =
      Core.List.iter ~f:(fun { last_modified;  S3.Ls.key; size; _ } -> Caml.Printf.eprintf "%s\t%d\t%s\n" (Time.to_string last_modified) size key ) result;

      match cont with
      | S3.Ls.More continuation -> ratelimit_f ()
        >>=? retry ~retries:5 ~delay:1.0 ~f:continuation
        >>=? ls_all
      | S3.Ls.Done -> Deferred.Or_error.return ()
    in
    Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
    let credentials = Core.Or_error.ok_exn credentials in
    (* nb client does not support redirects or preflight 100 *)
    ls_all ([], S3.Ls.More (fun () -> S3.ls ~credentials ?prefix ~bucket ())) >>= function
    | Result.Ok () -> return ()
    | Error _ -> return ()

  let exec ({ Cli.profile }, cmd) =
    match cmd with
    | Cli.Cp { src; dest } ->
      cp profile src dest ()
    | Rm path ->
      rm profile path ()
    | Ls { ratelimit; bucket; prefix } ->
      ls profile ratelimit bucket prefix ()

end
