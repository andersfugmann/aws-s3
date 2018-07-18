(** Module for abstracting async and lwt *)
module type Compat = sig
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

    module Infix: sig
      val (>>=): 'a t -> ('a -> 'b t) -> 'b t
      val (>>|): 'a t -> ('a -> 'b) -> 'b t
      val (>>=?): ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> ('b, 'c) result t
    end
  end

  module Pipe : sig

    type 'a writer

    type 'a reader

    val create_reader : f:('a writer -> unit Deferred.t) -> 'a reader
    val flush : 'a writer -> unit Deferred.t
    val write: 'a writer -> 'a -> unit Deferred.t
    val close: 'a writer -> unit
    val read: 'a reader -> 'a option Deferred.t
  end

  module Cohttp_deferred : sig
    module Body : sig
      type t
      val to_string: t -> string Deferred.t
      val of_string: string -> t
      val of_pipe: string Pipe.reader -> t
      val to_pipe: t -> string Pipe.reader
    end

    val call:
      ?headers:Cohttp.Header.t -> ?body:Body.t ->
      Cohttp.Code.meth -> Uri.t ->
      (Cohttp.Response.t * Body.t) Deferred.t
  end
end
