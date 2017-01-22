(**
 *  Values.
 *)


(** The type of values. *)
type t =
  | Boolean of bool
  | Integer of int
  | Primative of (t -> t)
  | Function of t Ident.Map.t * Ident.t * Abs_syntax_tree.exp

val to_string : t -> string
