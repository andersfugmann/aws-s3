module Make(Io : Types.Io) : sig
  open Io
  type t =
    | String of string
    | Empty
    | Chunked of { pipe : string Pipe.reader; length : int; chunk_size : int; }
  (**/**)
  val to_string : ?length:int -> string Pipe.reader -> string Deferred.t
  val read_string :
    length:int ->
    string option ->
    string Pipe.reader -> (string * string option) Deferred.Or_error.t

  val read_until :
    sep:string ->
    string Pipe.reader -> string -> (string * string) Deferred.Or_error.t
  val chunked_transfer : ?start:string -> string Pipe.reader -> string Pipe.writer -> string Deferred.Or_error.t
  val transfer: length:int -> ?start:string -> string Pipe.reader -> string Pipe.writer -> string Deferred.Or_error.t
  type body
  val reader: ?size:int -> unit -> body * string Pipe.writer
  val get: body -> string Deferred.Or_error.t

  val null: unit -> string Pipe.writer
  (**/**)
end
