open Core.Std
open Async.Std
open Aws_s3

type s3path = {bucket : string; objekt : string}

type cmd =
  | S3toLocal of string * string
  | LocaltoS3 of string * string

let determine_paths src dst =
  let is_s3 s = String.is_prefix ~prefix:"s3://" s in
  match is_s3 src, is_s3 dst with
  | (true, false) -> S3toLocal (String.drop_prefix src 5, dst)
  | (false, true) -> LocaltoS3 (src, String.drop_prefix dst 5)
  | (false, false) -> failwith "Use cp(1) :)"
  | (true, true) -> failwith "Does not support copying from s3 to s3"

let cp profile src dst () =
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  (* nb client does not support redirects or preflight 100 *)
  match determine_paths src dst with
  | S3toLocal (src, dst) ->
    begin
      S3.get ~credentials ~path:src () >>= function
      | Ok data ->
        Writer.with_file dst ~f:(fun writer -> Writer.write writer data; return ())
      | Error e -> Log.Global.error "Get error: %s" (Error.to_string_hum e);
        return ()
    end
  | LocaltoS3 (src, dst) ->
    Reader.file_contents src >>= fun data ->
    S3.put ~credentials ~path:dst data >>= function
    | Ok () -> return ()
    | Error e ->
      Log.Global.error "Could not put file: Error is: %s" (Error.to_string_hum e);
      return ()

let rm profile path () =
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  S3.delete ~credentials ~path () >>= function
    | Ok () -> return ()
    | Error e ->
      Log.Global.error "Could not delete file: Error is: %s" (Error.to_string_hum e);
      return ()

let ls profile bucket () =
  let rec ls_all (result, cont) =
    let open Deferred.Or_error in

    Core.Std.List.iter ~f:(fun { S3.Ls.key; size; _ } -> printf "%d\t%s\n" size key) result;

    match cont with
    | S3.Ls.More continuation -> continuation () >>= ls_all
    | S3.Ls.Done -> return ()
  in
  Credentials.Helper.get_credentials ?profile () >>= fun credentials ->
  let credentials = Or_error.ok_exn credentials in
  (* nb client does not support redirects or preflight 100 *)
  let open Deferred.Or_error in
  let res = S3.ls ~credentials ~path:bucket () >>= fun x -> ls_all x in
  let open Async.Std in
  res >>= function
  | Ok () -> return ()
  | Error e -> Log.Global.error "Error doing ls: Error is: %s" (Error.to_string_hum e);
      return ()

let () =
  let profile_flag = Command.Spec.(flag "profile" (optional string) ~doc:"profile") in

  let cp =
    Command.async
      ~summary:"Copy files to/from S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> anon ("src" %: string)
                    +> anon ("dst" %: string)
                   ) cp
  in
  let rm =
    Command.async
      ~summary:"Delete file in S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> anon ("path" %: string)
                   ) rm
  in
  let ls =
    Command.async
      ~summary:"List files in S3"
      Command.Spec.(empty
                    +> profile_flag
                    +> anon ("bucket" %: string)
                    ) ls
  in
  let s3_command = Command.group ~summary:"S3 command" [ "cp", cp; "rm", rm; "ls", ls ] in
  let command = Command.group ~summary:"Aws s3 command line utility" [ "s3", s3_command ] in

  Command.run command
