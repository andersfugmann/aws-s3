open !StdLabels [@@warning "-66"]
let sprintf = Printf.sprintf
open Protocol_conv_json

type time = float
let time_of_json_exn t =
  try
    Json.to_string t |> Time.parse_iso8601_string
  with
  | _ -> raise Json.(Protocol_error (make_error ~value:t "Not an iso8601 string"))

let%test "time conv" =
  time_of_json_exn (`String "2018-08-06T19:26:20Z") = 1533583580.

type t = {
  access_key: string [@key "AccessKeyId"];
  secret_key: string [@key "SecretAccessKey"];
  token: string option [@key "Token"];
  expiration: time option [@key "Expiration"];
} [@@deriving of_protocol ~driver:(module Json)]

let make ~access_key ~secret_key ?token ?expiration () =
  { access_key; secret_key; token; expiration }

module Make(Io : Types.Io) = struct
  module Http = Http.Make(Io)
  module Body = Body.Make(Io)
  open Io
  open Deferred

  module Iam = struct
    let instance_data_endpoint =
      let instance_data_host = "instance-data.ec2.internal" in
      let instance_region = Region.Other instance_data_host in
      Region.endpoint ~inet:`V4 ~scheme:`Http instance_region
    let get_role () =
      let path = "/latest/meta-data/iam/security-credentials/" in
      let body, sink =
        let reader, writer = Pipe.create () in
        Body.to_string reader, writer
      in
      Http.call ~endpoint:instance_data_endpoint ~path ~sink ~headers:Headers.empty `GET >>=? fun (status, message, _headers, error_body) ->
      match status with
      | code when code >= 200 && code < 300 ->
        body >>= fun body ->
        Deferred.Or_error.return body
      | _ ->
        let msg = sprintf "Failed to get role. %s. Reponse %s" message error_body in
        Deferred.Or_error.fail (Failure msg)

    let get_credentials role =
      let path = sprintf "/latest/meta-data/iam/security-credentials/%s" role in
      let body, sink =
        let reader, writer = Pipe.create () in
        Body.to_string reader, writer
      in
      Http.call ~endpoint:instance_data_endpoint ~path ~sink ~headers:Headers.empty `GET >>=? fun (status, message, _headers, error_body) ->
      match status with
      | code when code >= 200 && code < 300 ->
        body >>= fun body ->
        let json = Yojson.Safe.from_string body in
        Deferred.Or_error.catch (fun () -> of_json_exn json |> Deferred.Or_error.return)
      | _ ->
        let msg = sprintf "Failed to get credentials. %s. Reponse %s" message error_body in
        Deferred.Or_error.fail (Failure msg)
  end

  module Local = struct
    let get_credentials ?(profile="default") () =
      let home = Sys.getenv_opt "HOME" |> function Some v -> v | None -> "." in
      let creds_file = Printf.sprintf "%s/.aws/credentials" home in
      Deferred.Or_error.catch @@
      fun () ->
      let ini = new Inifiles.inifile creds_file in
      let access_key = ini#getval profile "aws_access_key_id" in
      let secret_key = ini#getval profile "aws_secret_access_key" in
      make ~access_key ~secret_key () |> Deferred.Or_error.return
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
end
