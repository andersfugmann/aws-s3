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
module Pipe = struct
  open Async_kernel
  open Deferred

  type 'a writer = 'a Pipe.Writer.t
  type 'a reader = 'a Pipe.Reader.t

  let flush writer = Pipe.downstream_flushed writer >>= fun _ -> return ()
  let read reader = Pipe.read reader >>= function
    | `Eof -> return None
    | `Ok v -> return (Some v)

  let write writer data = Pipe.write writer data
  let close writer = Pipe.close writer
  let create_reader ~f = Pipe.create_reader ~close_on_exception:true f
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_async.Body.t
    let to_string = Cohttp_async.Body.to_string
    let of_string = Cohttp_async.Body.of_string
    let of_pipe = Cohttp_async.Body.of_pipe
    let to_pipe = Cohttp_async.Body.to_pipe
  end

  let call ?headers ?body meth uri = Cohttp_async.Client.call ?headers ?body meth uri
end
