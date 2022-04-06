type key = string
type 'a t
val empty : 'a t
val is_empty : 'a t -> bool
val mem : key -> 'a t -> bool
val singleton : key -> 'a -> 'a t
val remove : key -> 'a t -> 'a t
val merge :
  (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val iter : (key -> 'a -> unit) -> 'a t -> unit
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val for_all : (key -> 'a -> bool) -> 'a t -> bool
val exists : (key -> 'a -> bool) -> 'a t -> bool
val filter : (key -> 'a -> bool) -> 'a t -> 'a t
val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
val cardinal : 'a t -> int
val bindings : 'a t -> (key * 'a) list
val find : key -> 'a t -> 'a
val find_opt : key -> 'a t -> 'a option
val find_first : (key -> bool) -> 'a t -> key * 'a
val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
val find_last : (key -> bool) -> 'a t -> key * 'a
val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
val change : key:key -> f:('a option -> 'a option) -> 'a t -> 'a t
val add : key:key -> value:'a -> 'a t -> 'a t
val add_opt : key:key -> value:'a option -> 'a t -> 'a t
val find_prefix : prefix:string -> 'a t -> (key * 'a) list
