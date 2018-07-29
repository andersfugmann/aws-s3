type t =
  | Ap_northeast_1
  | Ap_northeast_2
  | Ap_northeast_3
  | Ap_southeast_1
  | Ap_southeast_2
  | Ap_south_1
  | Eu_central_1
  | Cn_northwest_1
  | Cn_north_1
  | Eu_west_1
  | Eu_west_2
  | Eu_west_3
  | Sa_east_1
  | Us_east_1
  | Us_east_2
  | Us_west_1
  | Us_west_2
  | Ca_central_1
  | Other of string

val to_string : t -> string
val of_string : string -> t
val of_host : string -> t
val to_host : ?dualstack:bool -> t -> string
