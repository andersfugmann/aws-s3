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

    module Infix = struct
      let (>>=) : 'a t -> ('a -> 'b t) -> 'b t = fun v f ->
        v >>= function
        | Ok v -> f v
        | Error exn -> Async_kernel.return (Error exn)
    end
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
  let write writer data =
    (*Pipe.write writer data *)
    Pipe.write_without_pushback writer data;
    return ()
  let close writer = Pipe.close writer
  let close_reader reader = Pipe.close_read reader
  let create_reader ~f = Pipe.create_reader ~close_on_exception:true f
  let transfer reader writer =
    Pipe.transfer_id reader writer
  let create () = Pipe.create ()
  let closed ~f reader =
    don't_wait_for (Pipe.closed reader >>= fun () -> f ())
end

module Net = struct
  let lookup host =
    let open Async_unix.Unix in
    Addr_info.get ~host [ Addr_info.AI_FAMILY PF_INET
                        (* ; Addr_info.AI_FAMILY PF_INET6 *)
                        ; Addr_info.AI_SOCKTYPE SOCK_STREAM]
    >>= function
    | { Addr_info.ai_addr=ADDR_INET (addr, _); _ }::_ ->
      Deferred.Or_error.return (Ipaddr_unix.of_inet_addr addr)
    | _ -> Deferred.Or_error.fail (failwith ("Failed to resolve host: " ^ host))

  (* Dont need a port, as we only use 80 or 443 *)
  (* Need to return a handle to close the connection, as lwt_streams cannot be closed
     from the consumer end - Which would be preferable. - But we are not tied to use Lwt_streams. *)
  let connect ~host ~scheme =
    lookup host >>=? fun addr ->
    let endp = match scheme with
      | `Http -> `TCP (addr, 80)
      | `Https -> `OpenSSL (host, addr, 443)
    in
    Conduit_async.connect endp >>= fun (ic, oc) ->
    let reader = Reader.pipe ic in
    don't_wait_for (Async_kernel.Pipe.closed reader >>= fun () ->
                    Reader.close ic >>= fun () ->
                    Writer.close oc
                   );
    let writer = Writer.pipe oc in
    Deferred.Or_error.return (reader, writer)
end
