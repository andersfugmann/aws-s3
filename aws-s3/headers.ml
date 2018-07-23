include Map.Make(struct
    type t = string
    let compare a b = String.(compare (lowercase_ascii a) (lowercase_ascii b))
  end)

let change ~key ~f map =
  match f (find_opt key map) with
  | None -> remove key map
  | Some v -> add key v map

let add ~key ~value map = add key value map

let add_opt ~key ~value map =
  match value with
  | Some value -> add ~key ~value map
  | None -> map
