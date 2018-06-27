include Aws_s3.Types.Compat
  with type 'a Deferred.t = 'a Lwt.t
  with type Cohttp_deferred.Body.t = Cohttp_lwt.Body.t
