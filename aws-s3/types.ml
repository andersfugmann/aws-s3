(** Module for abstracting async and lwt *)
module type Io = sig
  module Deferred : sig
    type 'a t
    module Or_error: sig
      type nonrec 'a t = ('a, exn) result t
      val return: 'a -> 'a t
      val fail: exn -> 'a t
      val catch: (unit -> 'a t) -> 'a t
      module Infix: sig
        val (>>=): 'a t -> ('a -> 'b t) -> 'b t
      end
    end

    val return: 'a -> 'a t
    val after: float -> unit t
    val catch: (unit -> 'a t) -> 'a Or_error.t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val (>>|): 'a t -> ('a -> 'b) -> 'b t
    val (>>=?): ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> ('b, 'c) result t
  end

  module Pipe : sig
    type 'a writer
    type 'a reader
    val create_reader : f:('a writer -> unit Deferred.t) -> 'a reader
    val create : unit -> 'a reader * 'a writer
    val flush : 'a writer -> unit Deferred.t
    val write: 'a writer -> 'a -> unit Deferred.t
    val close: 'a writer -> unit
    val close_reader: 'a reader -> unit
    val read: 'a reader -> 'a option Deferred.t
    val transfer: 'a reader -> 'a writer -> unit Deferred.t
    val closed : f:(unit -> unit Deferred.t) -> 'a reader -> unit
  end

  module Net : sig
    val connect : host:string ->
      scheme:[< `Http | `Https ] ->
      (string Pipe.reader * string Pipe.writer) Deferred.Or_error.t
  end

end
