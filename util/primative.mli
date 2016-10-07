(** The type of binary operations *)
type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

(** A mapping from binary operations to their type *)
val tp_env : Type.t Ident.Map.t

(** Computes the string representation of a binary operator. *)
val binop_to_string : binop -> string

(** Computes the binary operator represented by a string. *)
val binop_of_string : string -> binop
