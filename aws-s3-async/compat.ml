open Async_kernel
module Deferred = struct
  type 'a t = 'a Async_kernel.Deferred.t

  module Or_error = struct
    type nonrec 'a t = ('a, exn) result t
    let return v = Async_kernel.Deferred.return (Ok v)
    let fail exn = Async_kernel.Deferred.return (Error exn)
    let catch f =
      Async_kernel.Monitor.try_with f >>= function
      | Ok v -> Async_kernel.return v
      | Error exn -> Async_kernel.return (Error exn)

    module Infix = struct
      let (>>=) : 'a t -> ('a -> 'b t) -> 'b t = fun v f ->
        v >>= function
        | Ok v -> f v
        | Error exn -> Async_kernel.return (Error exn)
    end
  end

  module Infix = struct
    let (>>=) = Async_kernel.(>>=)
    let (>>|) = Async_kernel.(>>|)
    let (>>=?) v f =
      v >>= function
      | Ok v -> f v
      | Error exn -> return (Error exn)
  end

  let return = Async_kernel.return
  let after delay = Async_kernel.after (Core_kernel.Time_ns.Span.of_sec delay)
  let catch f = Async_kernel.Monitor.try_with f
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_async.Body.t
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
