(** Module for abstracting async and lwt *)
module type Io = sig
  module Deferred : sig
    type 'a t
    module Or_error: sig
      type nonrec 'a t = ('a, exn) result t
      val return: 'a -> 'a t
      val fail: exn -> 'a t
      val catch: (unit -> 'a t) -> 'a t
      val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    end

    val return: 'a -> 'a t
    val after: float -> unit t
    val catch: (unit -> 'a t) -> 'a Or_error.t
    val async: unit t -> unit

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val (>>|): 'a t -> ('a -> 'b) -> 'b t
    val (>>=?): ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> ('b, 'c) result t
  end

  module Pipe : sig
    type ('a, 'b) pipe
    type writer_phantom
    type reader_phantom
    type 'a writer = ('a, writer_phantom) pipe
    type 'a reader = ('a, reader_phantom) pipe
    val create_reader : f:('a writer -> unit Deferred.t) -> 'a reader
    val create_writer : f:('a reader -> unit Deferred.t) -> 'a writer
    val create : unit -> 'a reader * 'a writer
    val flush : 'a writer -> unit Deferred.t
    val write: 'a writer -> 'a -> unit Deferred.t
    val close: 'a writer -> unit
    val close_reader: 'a reader -> unit
    val read: 'a reader -> 'a option Deferred.t
    val transfer: 'a reader -> 'a writer -> unit Deferred.t
    val is_closed: ('a, 'b) pipe -> bool
    val closed : ('a, 'b) pipe -> unit Deferred.t
  end

  module Net : sig
    val connect :
      domain:Unix.socket_domain ->
      host:string ->
      scheme:[< `Http | `Https ] ->
      (string Pipe.reader * string Pipe.writer) Deferred.Or_error.t
  end

end
