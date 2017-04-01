open Core.Std
open Async.Std

type s3path = {bucket : string; objekt : string}

type cmd =
    S3toLocal of string * string
  | LocaltoS3 of string * string

let determine_paths src dst =
  let is_s3 s = String.is_prefix ~prefix:"s3://" s in
  match is_s3 src, is_s3 dst with
  | (true, false) -> S3toLocal (String.drop_prefix src 5, dst)
  | (false, true) -> LocaltoS3 (src, String.drop_prefix dst 5)
  | (false, false) -> failwith "Use cp(1) :)"
  | (true, true) -> failwith "Does not support copying from s3 to s3"

let cp profile src dst () =
  S3.Credentials.get_credentials profile >>= fun credentials ->
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
    S3.put ~gzip:true ~credentials ~path:dst data >>= function
    | Ok () -> return ()
    | Error e ->
      Log.Global.error "Could not put file: Error is: %s" (Error.to_string_hum e);
      return ()

let () =
  Command.async
    ~summary:"Simple command line client that copies files to/from S3"
    Command.Spec.(empty
                  +> anon ("src" %: string)
                  +> anon ("dst" %: string)
                 ) (cp None)
  |> (fun x -> Command.run x)
