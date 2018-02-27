open Core
open Async
open Aws_s3

type objekt = { bucket: string; key: string }
let objekt_of_uri u = { bucket = (Option.value_exn ~message:"No Host in uri" (Uri.host u));
                        key = String.drop_prefix (Uri.path u) 1 (* Remove the beginning '/' *) }

type cmd =
  | S3toLocal of objekt * string
  | LocaltoS3 of string * objekt

let rec retry ~delay ~retries ~(f : (unit -> 'a Deferred.Or_error.t)) () : 'a Deferred.Or_error.t =
  f () >>= function
  | Result.Error e when retries > 0 ->
      Log.Global.info "Retry on error: %s" (Error.to_string_hum e);
      after delay >>= fun () -> retry ~delay ~retries:(retries - 1) ~f ()
  | r -> return r

let determine_paths src dst =
  let src = Uri.of_string src in
  let dst = Uri.of_string dst in
  let is_s3 u = Uri.scheme u = Some "s3" in
  match is_s3 src, is_s3 dst with
  | (true, false) -> S3toLocal (objekt_of_uri src, Uri.path dst)
  | (false, true) -> LocaltoS3 (Uri.path src, objekt_of_uri dst)
  | (false, false) -> failwith "Use cp(1) :)"
  | (true, true) -> failwith "Does not support copying from s3 to s3"

let cp profile src dst () =
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  (* nb client does not support redirects or preflight 100 *)
  match determine_paths src dst with
  | S3toLocal (src, dst) ->
    begin
      S3.get ~credentials ~bucket:src.bucket ~key:src.key () >>= function
      | Ok data ->
        Writer.with_file dst ~f:(fun writer -> Writer.write writer data; return ())
      | Error e -> Log.Global.error "Get error: %s" (Error.to_string_hum e);
        return ()
    end
  | LocaltoS3 (src, dst) ->
    Reader.file_contents src >>= fun data ->
    S3.put ~credentials ~bucket:dst.bucket ~key:dst.key data >>= function
    | Ok () -> return ()
    | Error e ->
      Log.Global.error "Could not put file: Error is: %s" (Error.to_string_hum e);
      return ()

let rm profile path () =
  let objekt = Uri.of_string path |> objekt_of_uri in
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  S3.delete ~credentials ~bucket:objekt.bucket ~key:objekt.key () >>= function
    | Ok () -> return ()
    | Error e ->
      Log.Global.error "Could not delete file: Error is: %s" (Error.to_string_hum e);
      return ()

let ls profile ratelimit bucket prefix () =
  let ratelimit_f = match ratelimit with
    | None -> fun () -> Deferred.Or_error.return ()
    | Some n -> fun () -> after (Time.Span.of_ns(1000. /. float n)) >>= Deferred.Or_error.return
  in
  let rec ls_all (result, cont) =
    let open Deferred.Or_error in

    Core.List.iter ~f:(fun { S3.Ls.key; size; _ } -> printf "%d\t%s\n" size key) result;

    match cont with
    | S3.Ls.More continuation -> ratelimit_f () >>= retry ~retries:5 ~delay:(Time.Span.of_sec 1.0) ~f:continuation >>= ls_all
    | S3.Ls.Done -> return ()
  in
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  (* nb client does not support redirects or preflight 100 *)
  ls_all ([], S3.Ls.More (fun () -> S3.ls ~credentials ?prefix ~bucket ())) >>= function
  | Result.Ok () -> return ()
  | Error e -> Log.Global.error "Error doing ls: Error is: %s" (Error.to_string_hum e);
      return ()

let () =
  let profile_flag = Command.Spec.(flag "profile" (optional string) ~doc:"<profile> Use local credentials from <profile>") in
  let rate_limit_flag = Command.Spec.(flag "ratelimit" (optional int) ~doc:"<N> ratelimit requests to N/sec") in

  let cp =
    Command.async
      ~summary:"Copy files to/from S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> anon ("src" %: string)
                    +> anon ("dst" %: string)
                    |> Fn.flip to_param cp)
  in
  let rm =
    Command.async
      ~summary:"Delete file in S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> anon (* (sequence) *) ("path" %: string)
                    |> Fn.flip to_param rm)
  in
  let ls =
    Command.async
      ~summary:"List files in S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> rate_limit_flag
                    +> anon ("bucket" %: string)
                    +> anon (maybe ("prefix" %: string))
                    |> Fn.flip to_param ls)
  in
  let s3_command = Command.group ~summary:"S3 command" [ "cp", cp; "rm", rm; "ls", ls ] in
  let command = Command.group ~summary:"Aws s3 command line utility" [ "s3", s3_command ] in

  Command.run command
