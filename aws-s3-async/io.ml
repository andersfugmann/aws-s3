open Async
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

    let (>>=) : 'a t -> ('a -> 'b t) -> 'b t = fun v f ->
      v >>= function
      | Ok v -> f v
      | Error exn -> Async_kernel.return (Error exn)
  end

  let (>>=) = Async_kernel.(>>=)
  let (>>|) = Async_kernel.(>>|)
  let (>>=?) v f =
    v >>= function
    | Ok v -> f v
    | Error exn -> return (Error exn)

  let return = Async_kernel.return
  let after delay = Async_kernel.after (Core_kernel.Time_ns.Span.of_sec delay)
  let catch f = Async_kernel.Monitor.try_with f
  let async = don't_wait_for
end

module Ivar = struct
  type 'a t = 'a Async.Ivar.t
  let create () = Async.Ivar.create ()
  let fill t v = Async.Ivar.fill t v
  let wait t = Async.Ivar.read t
end

module Pipe = struct
  open Async_kernel
  open Deferred

  type ('a, 'b) pipe = ('a, 'b) Pipe.pipe
  type writer_phantom = Pipe.Writer.phantom
  type reader_phantom = Pipe.Reader.phantom
  type 'a writer = 'a Pipe.Writer.t
  type 'a reader = 'a Pipe.Reader.t

  let flush writer = Pipe.downstream_flushed writer >>= fun _ -> return ()
  let read reader = Pipe.read reader >>= function
    | `Eof -> return None
    | `Ok v -> return (Some v)
  let write writer data =
    (* Pipe.write writer data *)
    Pipe.write_without_pushback writer data;
    return ()
  let close writer = Pipe.close writer
  let close_reader reader = Pipe.close_read reader
  let create_reader ~f = Pipe.create_reader ~close_on_exception:true f
  let create_writer ~f = Pipe.create_writer f
  let transfer reader writer = Pipe.transfer_id reader writer
  let create () = Pipe.create ()
  let is_closed pipe = Pipe.is_closed pipe
  let closed pipe = Pipe.closed pipe
end

module Net = struct
  let connect ?connect_timeout_ms ~inet ~host ~port ~scheme =
    let uri =
      let scheme = match scheme with
        | `Http -> "http"
        | `Https -> "https"
      in
      Uri.make ~scheme ~host:host ~port ()
    in
    let options =
      let domain : Async_unix.Unix.socket_domain =
        match inet with
          | `V4 -> PF_INET
          | `V6 -> PF_INET6
      in
      Core.Unix.[AI_FAMILY domain]
    in
    let close_socket_no_error = function
      | Conduit_async.V3.Inet_sock socket -> try Socket.shutdown socket `Both; with _ -> ()
    in
    let interrupt = match connect_timeout_ms with
      | None -> None
      | Some ms -> Some (Async.after (Core.Time.Span.of_ms (float ms)))
    in
    Async.try_with (fun () -> Conduit_async.V3.connect_uri ?interrupt ~options uri) >>=? fun (socket, ic, oc) ->
    let reader = Reader.pipe ic in
    don't_wait_for (
      Async_kernel.Pipe.closed reader >>= fun () ->
      Monitor.try_with ~name:"Io.Net.connect connection-cleanup" (fun () ->
        Writer.close oc >>= fun () ->
        Reader.close ic >>= fun () ->
        return ()
      ) >>= fun _ ->
      close_socket_no_error socket;
      return ()
    );
    let writer = Writer.pipe oc in
    Deferred.Or_error.return (reader, writer)
end
