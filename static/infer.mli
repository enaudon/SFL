(** Type Inference *)

val infer : Parse_tree.top list -> Type_tree.top list

(* Debugging *)
val top_to_string : Unify.term Abs_syntax_tree.top -> string
val constraint_to_string : Unify.term * Unify.term -> string
