(* So we need to override something in the signature. We could also just delete the mli. *)
include Aws_s3.Types.Compat with type 'a Deferred.t = 'a Lwt.t
