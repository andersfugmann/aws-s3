open Lwt.Infix
module Deferred = struct
  type 'a t = 'a Lwt.t

  module Or_error = struct
    type nonrec 'a t = ('a, exn) Lwt_result.t
    let return = Lwt_result.return
    let fail = Lwt_result.fail
    let catch : (unit -> 'a t) -> 'a t = fun f ->
      Lwt.catch f Lwt_result.fail

    module Infix = struct
      let (>>=) a b =
        a >>= function
        | Ok v -> b v
        | Error _ as err -> Lwt.return err
    end
  end

  module Infix = struct
    let (>>=) = Lwt.Infix.(>>=)
    let (>>|) a b = a >>= fun v -> Lwt.return (b v)
    let (>>=?) = Lwt_result.Infix.(>>=)
  end

  let return = Lwt.return
  let after delay = Lwt_unix.sleep delay
  let catch f =
    Lwt.catch
      (fun () -> f () >>= Or_error.return)
      (fun exn -> Or_error.fail exn)
end

module Pipe = struct
  open Lwt.Infix
  type 'a elem = Flush of unit Lwt_mvar.t
               | Data of 'a

  type 'a reader = 'a Lwt_stream.t
  type 'a writer = { cond: unit Lwt_condition.t; queue: 'a elem Queue.t; mutable closed: bool }

  let write writer data =
    Queue.add (Data data) writer.queue;
    if Queue.length writer.queue = 1 then Lwt_condition.signal writer.cond ();
    Lwt.return_unit

  let flush writer =
    let mvar = Lwt_mvar.create_empty () in
    Queue.add (Flush mvar) writer.queue;
    if Queue.length writer.queue = 1 then Lwt_condition.signal writer.cond ();
    Lwt_mvar.take mvar

  let close writer =
    writer.closed <- true;
    Lwt_condition.signal writer.cond ()

  let create_reader: f:('a writer -> unit Lwt.t) -> 'a reader = fun ~f ->
    let writer = { cond = Lwt_condition.create (); queue = Queue.create (); closed = false; } in
    let rec producer: unit -> 'a option Lwt.t = fun () ->
      match Queue.pop writer.queue with
      | Data data -> Lwt.return (Some data)
      | Flush mvar ->
        Lwt_mvar.put mvar () >>= fun () ->
        producer ()
      | exception Queue.Empty -> begin
          match writer.closed with
          | true -> Lwt.return None
          | false ->
            Lwt_condition.wait writer.cond >>= fun () ->
            producer ()
        end
    in
    Lwt.async (fun () -> f writer >>= fun () -> close writer; Lwt.return_unit);
    Lwt_stream.from producer
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_lwt.Body.t
    let to_string = Cohttp_lwt.Body.to_string
    let of_string = Cohttp_lwt.Body.of_string
    let to_pipe = Cohttp_lwt.Body.to_stream
    let of_pipe = Cohttp_lwt.Body.of_stream
  end

  let call ?headers ?body meth uri =
    Cohttp_lwt_unix.Client.call ?headers ?body meth uri
end
