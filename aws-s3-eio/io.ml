module Deferred = struct
  type 'a t = 'a
  module Or_error = struct
    type nonrec 'a t = ('a, exn) result t
    let return v = Ok v
    let fail exn = Error exn
    let catch f =
      match f () with
      | v -> v
      | exception exn -> Error exn

    let (>>=) : 'a t -> ('a -> 'b t) -> 'b t = fun v f ->
      match v with
      | Ok v -> f v
      | err -> err
  end

  let (>>=) v f = f v
  let (>>|) v f = f v
  let (>>=?) v f =
    match v with
    | Ok v -> f v
    | err -> err

  let return v = v
  let after delay =
    (* Need some Eio function to delay:
       Eio.Time.Sleep time_state (float delay)
    *)
    failwith "Not implemented"
  let catch f = match f () with
    | v -> Ok v
    | exception exn -> Error exn

  (* Need some state to be able to spawn a new fiber *)
  let async = (* Eio spawn a new fiber *) failwith "Not implemented"
end

module Ivar = struct
  type 'a t = ('a Eio.Promise.t * 'a Eio.Promise.u)
  let create () = Eio.Promise.create ()
  let fill (_t, u) v = Eio.Promise.resolve u v
  let wait (t, _u) = Eio.Promise.await t
end

module Pipe = struct
  (* Create an infinite pipe that can be closed.
     Then the reader is closed any new writes will fail
     When the writer is closed the read can read new message until last element is read
     The pipe uses callbacks????
  *)

  type ('a, 'b) pipe = ('a, 'b) Pipe.pipe
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
  let connect ?connect_timeout_ms ~inet ~host ~port ~scheme () =
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
      Core_unix.[AI_FAMILY domain]
    in
    let close_socket_no_error = function
      | Conduit_async.V3.Inet_sock socket -> try Socket.shutdown socket `Both; with _ -> ()
    in
    let interrupt = match connect_timeout_ms with
      | None -> None
      | Some ms -> Some (Async.after (Time_float_unix.Span.of_int_ms ms))
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
