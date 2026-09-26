(**/**)
type meth = Types.meth

val string_of_method : meth -> string

(** Limits on the idle connections kept by {!Make_pooled}. *)
module type Pool_config = sig
  (** Idle connections kept per (scheme, host, port). *)
  val max_idle_per_host : int

  (** Idle connections kept across all endpoints. Without it a caller sweeping
      many endpoints holds a descriptor per endpoint until the process exits,
      since an endpoint that is never revisited is never swept. Reaching it
      drops the oldest idle connection in the pool. *)
  val max_idle_total : int

  (** Servers reap idle keep-alive connections on their own schedule (Backblaze
      B2 aggressively so), and handing out one the peer has already closed costs
      a failed request, so how long an entry may sit in the pool is
      peer-dependent. *)
  val max_idle_age : float
end

module Default_pool_config : Pool_config

(** Http client reusing idle connections across requests within the limits of
    [Config]. Each application of the functor has its own pool. *)
module Make_pooled : functor(Io : Types.Io) -> functor(Config : Pool_config) ->
  Types.Http with module Io = Io

module Make : functor(Io : Types.Io) -> Types.Http with module Io = Io
(**/**)
