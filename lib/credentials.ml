module R = Result
open Core
open Async
open Cohttp
open Cohttp_async

type time = Time.t
let time_of_yojson = function
  | `String s -> R.Ok (Time.of_string s)
  | _ -> R.Error "Expected string"

type t = {
  aws_access_key: string [@key "AccessKeyId"];
  aws_secret_key: string [@key "SecretAccessKey"];
  aws_token: string option [@key "Token"];
  expiration: time option [@key "Expiration"];
  } [@@deriving of_yojson { strict = false }]

let make_credentials ~access_key ~secret_key ?token ?expiration () =
  { aws_access_key=access_key; aws_secret_key=secret_key; aws_token=token; expiration }

module Iam = struct
  let instance_data_host = "instance-data.ec2.internal"
  let get_role () =
    let inner () =
      let uri = Uri.make ~host:instance_data_host ~scheme:"http" ~path:"/latest/meta-data/iam/security-credentials/" () in
      let request = Cohttp.Request.make ~meth:`GET uri in
      Cohttp_async.Client.request request >>= fun (response, body) ->
      match Cohttp.Response.status response with
      | #Code.success_status ->
          Body.to_string body >>= fun body ->
          return (Ok body)
      | _ ->
          Body.to_string body >>= fun body ->
          return (Or_error.errorf "Failed to get role from %s. Response was: %s" (Uri.to_string uri) body)
    in
    Deferred.Or_error.try_with_join inner

  let get_credentials role =
    let inner () =
      let path = sprintf "/latest/meta-data/iam/security-credentials/%s" role in
      let uri = Uri.make ~host:instance_data_host ~scheme:"http" ~path () in
      let request = Cohttp.Request.make ~meth:`GET uri in
      Cohttp_async.Client.request request >>= fun (response, body) ->
      match Cohttp.Response.status response with
      | #Code.success_status -> begin
          Body.to_string body >>= fun body ->
          let json = Yojson.Safe.from_string body in
          match (of_yojson json) with
          | R.Ok t ->
              return (Ok t)
          | R.Error s ->
              return (Or_error.errorf "Unable to parse credentials. Error was: %s" s)
        end
      | _ ->
          Body.to_string body >>= fun body ->
          return (Or_error.errorf "Failed to get credentials from %s. Response was: %s" (Uri.to_string uri) body)
    in
    Deferred.Or_error.try_with_join inner
end

module Local = struct
  let get_credentials ?(profile="default") () =
    let home = Sys.getenv "HOME" |> Option.value ~default:"." in
    let creds_file = sprintf "%s/.aws/credentials" home in
    Deferred.Or_error.try_with ~name:creds_file ~extract_exn:false @@
    fun () ->
    let ini = new Inifiles.inifile creds_file in
    let access_key = ini#getval profile "aws_access_key_id" in
    let secret_key = ini#getval profile "aws_secret_access_key" in
    make_credentials ~access_key ~secret_key () |> return
end

module Helper = struct
  let get_credentials ?profile () =
    match profile with
    | Some profile -> Local.get_credentials ~profile ()
    | None -> begin
        Local.get_credentials ~profile:"default" () >>= function
        | Result.Ok c -> Deferred.Or_error.return c
        | Error _ ->
            Iam.get_role () >>=? fun role ->
            Iam.get_credentials role
      end
end
