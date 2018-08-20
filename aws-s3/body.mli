module Make(Io : Types.Io) : sig
  open Io
  type t =
    | String of string
    | Empty
    | Chunked of { pipe : string Pipe.reader; length : int; chunk_size : int; }
  (**/**)
  val to_string :
    string Pipe.reader ->
    string Deferred.t

  val read_string :
    ?start:string ->
    length:int ->
    string Pipe.reader -> (string * string option) Deferred.Or_error.t

  val read_until :
    msg:string ->
    ?start:string ->
    sep:string ->
    string Pipe.reader -> (string * string option) Deferred.Or_error.t

  val chunked_transfer :
    ?start:string ->
    string Pipe.reader ->
    string Pipe.writer ->
    string option Deferred.Or_error.t

  val transfer:
    ?start:string ->
    length:int ->
    string Pipe.reader ->
    string Pipe.writer ->
    string option Deferred.Or_error.t

  val null: unit -> string Pipe.writer
  (**/**)
end
