type t = string

let compare = compare

let of_string str = str
let to_string id = id

type _t = t
module S = struct
  type t = _t
  let compare = compare
end

module type SET = Set.S with type elt = t
module Set = Set.Make(S)

module type MAP = Map.S with type key = t
module Map = Map.Make(S)
