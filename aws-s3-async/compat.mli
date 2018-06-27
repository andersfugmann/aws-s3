(**/**)
include Aws_s3.Types.Compat
  with type 'a Deferred.t = 'a Async.Deferred.t
  with type Cohttp_deferred.Body.t = Cohttp_async.Body.t
(**/**)
