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

  (** Module mimicking Async.Pipe *)
  module Pipe : sig
    (** Generic pipe *)
    type ('a, 'b) pipe

    (**/**)
    type writer_phantom
    type reader_phantom

    (**/**)
    type 'a writer = ('a, writer_phantom) pipe
    type 'a reader = ('a, reader_phantom) pipe

    (** Create a reader given a function f that fills the reader. Once f completes, the reader is closed *)
    val create_reader : f:('a writer -> unit Deferred.t) -> 'a reader

    (** Create a writer given a function f that reads off the writer. Once f completes, the writer is closed *)
    val create_writer : f:('a reader -> unit Deferred.t) -> 'a writer

    (** Create a reader/writer pipe. Data written to the reader can be read by the writer.
        Closing one end will close both ends. *)
    val create : unit -> 'a reader * 'a writer

    (** Flush a writer. The result we be determined once all elements in the pipe has been consumed *)
    val flush : 'a writer -> unit Deferred.t

    (** Write to a writer. If the writer is closed, the function raises an exception *)
    val write: 'a writer -> 'a -> unit Deferred.t

    (** Close a writer *)
    val close: 'a writer -> unit

    (** Close a reader *)
    val close_reader: 'a reader -> unit

    (** Read one element from a reader. The function will block until an element becomes available or the
        reader is closed, in which case [None] is returned *)
    val read: 'a reader -> 'a option Deferred.t

    (** Transfer all data from the reader to the writer. The function becomes determined when the reader or writer is closed *)
    val transfer: 'a reader -> 'a writer -> unit Deferred.t

    (** Return the state of a pipe *)
    val is_closed: ('a, 'b) pipe -> bool

    (** Wait for a pipe to be closed. The function is determined once the pipe is closed.
        the function can be called multiple times.

        Note that not all elements may have been consumed yet.
    *)
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
