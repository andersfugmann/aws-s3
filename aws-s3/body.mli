module Make(Io : Types.Io) : sig
  open Io
  open Deferred
  type t = string Pipe.reader
  type body =
    | String of string
    | Empty
    | Chunked of { pipe : string Pipe.reader; length : int; chunk_size : int; }
  (**/**)
  val to_string : ?length:int -> string Pipe.reader -> string Deferred.t
  val read_string :
    length:int ->
    string option ->
    string Pipe.reader -> (string * string option) Or_error.t

  val read_until :
    sep:string ->
    string Pipe.reader -> string -> (string * string) Deferred.Or_error.t
  val chunked_transfer : ?start:string -> string Pipe.reader -> string Pipe.writer -> string Deferred.Or_error.t
  val transfer: length:int -> ?start:string -> string Pipe.reader -> string Pipe.writer -> string Deferred.Or_error.t
  type string_body
  val reader: ?size:int -> unit -> string_body * string Pipe.writer
  val get: string_body -> string Deferred.Or_error.t

  val null: unit -> string Pipe.writer
  (**/**)
end
