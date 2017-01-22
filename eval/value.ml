type t =
  | Boolean of bool
  | Integer of int
  | Primative of (t -> t)
  | Function of t Ident.Map.t * Ident.t * Abs_syntax_tree.exp

let format ff v = match v with
  | Boolean b -> Format.fprintf ff "%b" b
  | Integer i -> Format.fprintf ff "%i" i
  | Primative _ -> Format.fprintf ff "<primative>"
  | Function _ -> Format.fprintf ff "<function>"

let to_string = Format.asprintf "%a" format
