(** Type Inference *)

val infer : Parse_tree.top list -> Type_tree.exp list

(* Debugging *)
val exp_to_string : Unify.term Abs_syntax_tree.exp -> string
val constraint_to_string : Unify.term * Unify.term -> string
