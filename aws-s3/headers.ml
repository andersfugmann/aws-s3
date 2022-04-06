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

let find_prefix ~prefix map =
  let prefix_length = String.length prefix in
  let has_prefix s =
    match prefix_length <= String.length s with
    | false -> false
    | true ->
      let rec inner = function
        | 0 -> true
        | n ->
          let i = n - 1 in
          match Char.equal prefix.[i] s.[i] with
          | true -> inner (i)
          | false -> false
      in
      inner (String.length prefix)
  in
  fold (fun key v acc -> match has_prefix key with
    | true ->
      let key = String.sub key prefix_length (String.length key - prefix_length) in
      (key, v) :: acc
    | false -> acc) map []
