module TypeVariable = struct
  type t = string * int

  let ht = Hashtbl.create 128
  let reset () = Hashtbl.clear ht
  let create name =
    try
      let i = Hashtbl.find ht name in
      Hashtbl.replace ht name (i + 1);
      (name, i)
    with Not_found ->
      Hashtbl.add ht name 1;
      (name, 0)

  let to_string (n, i) = Printf.sprintf "%s%d" n i
  let name (n, _) = n

  type _t = t
  module S = struct
    type t = _t
    let compare = compare
  end

  module type SET = Set.S with type elt = t
  module Set = Set.Make(S)

  module type MAP = Map.S with type key = t
  module Map = Map.Make(S)
end

module TV = TypeVariable


type t =
  | Unit
  | Boolean
  | Integer
  | Variable of TV.t
  | Function of t * t
  | Tuple of t list

let rec to_string tp = match tp with
  | Unit ->
    Printf.sprintf "Unit"
  | Boolean ->
    Printf.sprintf "Boolean"
  | Integer ->
    Printf.sprintf "Integer"
  | Variable (tv) ->
    Printf.sprintf "%s" (TV.to_string tv)
  | Function (Function (_, _) as t1, t2) ->
    Printf.sprintf "(%s) -> %s" (to_string t1) (to_string t2)
  | Function (t1, t2) ->
    Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)
  | Tuple ts ->
    Printf.sprintf "(%s)"
      (String.concat " & " (List.map to_string ts))
