(** Module for abstracting async and lwt *)
module type Compat = sig
  module Deferred : sig
    type 'a t
    module Or_error: sig
      type nonrec 'a t = ('a, Base.Error.t) result t
      val return: 'a -> 'a t
      val fail: Base.Error.t -> 'a t
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
      val (>>=?): ('a, 'c) result t -> ('a -> ('b, 'c) result t ) -> ('b, 'c) result t
    end
  end

  module Cohttp_deferred : sig
    module Body : sig
      type t
      val to_string: t -> string Deferred.t
      val of_string: string -> t
    end

    module Client : sig
      val request :
        ?body:Body.t ->
        Cohttp.Request.t ->
        (Cohttp.Response.t * Body.t) Deferred.t
    end
  end
end

(*
module Deferred_or_error (Deferred: Compat.Deferred) = struct
  open Deferred
  type nonrec 'a t = ('a, Base.Error.t) result t
  let return v = Result.Ok v |> return
  let fail e = Result.Error e |> return
  let (>>=) a b = a >>= function
    | Error _ as e -> return e
    | Ok v -> b v
end
*)
