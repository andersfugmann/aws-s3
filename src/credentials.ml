open Core.Std
open Async.Std
open Cohttp
open Cohttp_async

type time = Time.t
let time_of_yojson = function
  | `String s -> Pervasives.Ok (Time.of_string s)
  | _ -> Pervasives.Error "Expected string"

type t = {
  aws_access_key: string [@key "AccessKeyId"];
  aws_secret_key: string [@key "SecretAccessKey"];
  aws_token: string option [@key "Token"];
  expiration: time option [@key "Expiration"];
} [@@deriving of_yojson { strict = false }]

let get_role () =
  let inner () =
    let uri = Uri.make ~host:"169.254.169.254" ~scheme:"http" ~path:"/latest/meta-data/iam/security-credentials/" () in
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


let get_role_credentials role =
  let inner () =
    let path = sprintf "/latest/meta-data/iam/security-credentials/%s" role in
    let uri = Uri.make ~host:"169.254.169.254" ~scheme:"http" ~path () in
    let request = Cohttp.Request.make ~meth:`GET uri in
    Cohttp_async.Client.request request >>= fun (response, body) ->
    match Cohttp.Response.status response with
    | #Code.success_status -> begin
        Body.to_string body >>= fun body ->
        let json = Yojson.Safe.from_string body in
        match (of_yojson json) with
        | Pervasives.Ok t ->
            return (Ok t)
        | Pervasives.Error s ->
            return (Or_error.errorf "Unable to parse credentials. Error was: %s" s)
      end
    | _ ->
        Body.to_string body >>= fun body ->
        return (Or_error.errorf "Failed to get credentials from %s. Response was: %s" (Uri.to_string uri) body)
  in
  Deferred.Or_error.try_with_join inner

let get_file_credentials section =
  let home = Sys.getenv "HOME" |> Option.value ~default:"." in
  let creds_file = sprintf "%s/.aws/credentials" home in
  try
    let ini = new Inifiles.inifile creds_file in
    let aws_access_key = ini#getval section "aws_access_key_id" in
    let aws_secret_key = ini#getval section "aws_secret_access_key" in
    Some { aws_access_key; aws_secret_key; aws_token=None; expiration=None }
  with
  | _ -> None

(** Get credentials. If a profile is supplied the credentials is read from
    ~/.aws/credetils

    If not section is given, credetials is first read from section 'default',
    and if not found the credentials is looked up by machine role
*)

let get_credentials profile =
  let open Deferred.Or_error in
  match profile with
  | Some p -> begin
      match get_file_credentials p with
      | None -> errorf "No credentials found for profile: %s" p
      | Some c -> return c
    end
  | None -> begin
      match get_file_credentials "default" with
      | None ->
          get_role () >>= fun role ->
          get_role_credentials role
      | Some c -> return c
    end
