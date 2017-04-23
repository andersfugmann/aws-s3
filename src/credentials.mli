(** The credentials module handles reading credentials locally
    or from IAM service (aws).
*)

type t = {
  aws_access_key: string;
  aws_secret_key: string;
  aws_token: string option;
  expiration: Core.Std.Time.t option;
}


(** Make credentials *)
val make_credentials :
  access_key:string -> secret_key:string ->
  ?token:string -> ?expiration:Core.Std.Time.t -> unit -> t

module Iam : sig

  (** Get role assigned to this machine though IAM *)
  val get_role : unit -> string Async.Std.Deferred.Or_error.t

  (** Retrieve a credentials for [role] *)
  val get_credentials : string -> t Async.Std.Deferred.Or_error.t

end

module Local : sig
  (** Load credentials from ~/.aws/credentials (file format compatible
      with botocore). *)
  val get_credentials :
    ?profile:string -> unit -> t Async.Std.Deferred.Or_error.t
end

module Helper : sig

  (** Get credentials locally or though IAM service.
      If a profile is supplied the credentials is
      read from ~/.aws/credetils. If not section is given, credetials
      is first read from section 'default', and if not found the
      credentials is retrieved though looked up Iam aws credential
      system
  *)
  val get_credentials :
    ?profile:string -> unit -> t Async.Std.Deferred.Or_error.t
end
