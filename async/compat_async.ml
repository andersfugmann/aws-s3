type 'a deferred = 'a Async.Deferred.t
module Deferred = struct
  type 'a t = 'a deferred

  module Or_error = struct
    type nonrec 'a t = 'a Async.Deferred.Or_error.t
    let return = Async.Deferred.Or_error.return
    let fail = Async.Deferred.Or_error.fail
    let catch f = Async.Deferred.Or_error.try_with_join f

    module Infix = struct
      let (>>=) = Async.Deferred.Or_error.(>>=)
    end
  end

  module Infix = struct
    let (>>=) = Async.(>>=)
    let (>>|) = Async.(>>|)
    let (>>=?) = Async.(>>=?)
  end

  let return = Async.return
  let after delay = Async.after (Core.Time.Span.of_sec delay)
  let catch f = Async.Monitor.try_with_or_error f
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_async.Body.t
    let to_string = Cohttp_async.Body.to_string
    let of_string = Cohttp_async.Body.of_string
  end

  module Client = struct
    let request ?body request =
      Cohttp_async.Client.request ?body request
  end
end
