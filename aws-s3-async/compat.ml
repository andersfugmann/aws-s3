module Deferred = struct
  type 'a t = 'a Async_kernel.Deferred.t

  module Or_error = struct
    type nonrec 'a t = 'a Async_kernel.Deferred.Or_error.t
    let return = Async_kernel.Deferred.Or_error.return
    let fail = Async_kernel.Deferred.Or_error.fail
    let catch f = Async_kernel.Deferred.Or_error.try_with_join f

    module Infix = struct
      let (>>=) = Async_kernel.Deferred.Or_error.(>>=)
    end
  end

  module Infix = struct
    let (>>=) = Async_kernel.(>>=)
    let (>>|) = Async_kernel.(>>|)
    let (>>=?) = Async_kernel.(>>=?)
  end

  let return = Async_kernel.return
  let after delay = Async_kernel.after (Core_kernel.Time_ns.Span.of_sec delay)
  let catch f = Async_kernel.Monitor.try_with_or_error f
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_async.Body.t
    let empty = Cohttp_async.Body.empty
    let to_string = Cohttp_async.Body.to_string
    let of_string = Cohttp_async.Body.of_string
  end

  module Client = struct
    let request ~scheme ?body request =
      let scheme_str = match scheme with
        | `Http -> "http";
        | `Https -> "https";
      in
      let uri = Uri.with_uri ~scheme:(Some scheme_str) (Cohttp.Request.uri request) in
      Cohttp_async.Client.call
        ~headers:(Cohttp.Request.headers request)
        ?body
        (Cohttp.Request.meth request)
        uri
  end
end
