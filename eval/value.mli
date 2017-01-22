(**
 *  Values.
 *)


(** The type of values. *)
type t =
  | Boolean of bool
  | Integer of int
  | Function of (t -> t)

val to_string : t -> string
