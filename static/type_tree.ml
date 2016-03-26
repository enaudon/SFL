(*
 *  Type Tree
 *)


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

  let to_string (n, i) = n ^ (string_of_int i)

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


module AST = Abs_syntax_tree


type typo =
  | Boolean
  | Integer
  | Variable of TV.t
  | Function of typo * typo
  | Product of typo list
  | Disjunction of typo list

type t = typo AST.exp

let to_typo tt = AST.data tt

let rec typo_to_string tp = match tp with
  | Boolean ->
    Printf.sprintf "Boolean"
  | Integer ->
    Printf.sprintf "Integer"
  | Variable (tv) ->
    Printf.sprintf "%s" (TV.to_string tv)
  | Function (Function (_, _) as t1, t2) ->
    Printf.sprintf "(%s) -> %s" (typo_to_string t1) (typo_to_string t2)
  | Function (t1, t2) ->
    Printf.sprintf "%s -> %s" (typo_to_string t1) (typo_to_string t2)
  | Product ts ->
    Printf.sprintf "(%s)"
      (String.concat " & " (List.map typo_to_string ts))
  | Disjunction (ts) ->
    let strs = List.map typo_to_string ts in
    Printf.sprintf "{%s}" (String.concat ", " strs)

let rec literal_to_string l = match l with
  | AST.Boolean b -> string_of_bool b
  | AST.Integer i -> string_of_int i
  | AST.Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map to_string es))

and to_string tt = match tt with
  | AST.Variable (id, tp) ->
    Printf.sprintf "%s : %s" id (typo_to_string tp)
  | AST.Literal (l, tp) ->
    Printf.sprintf "%s : %s" (literal_to_string l) (typo_to_string tp)
  | AST.Application (e1, e2, tp) ->
    Printf.sprintf "((%s %s) : %s)"
      (to_string e1)
      (to_string e2)
      (typo_to_string tp)
  | AST.Binding (binds, e, _) ->
    let fn (id, e) = Printf.sprintf "%s = %s" id (to_string e) in
    Printf.sprintf "let %s in %s"
      (String.concat "; " (List.map fn binds))
      (to_string e)
