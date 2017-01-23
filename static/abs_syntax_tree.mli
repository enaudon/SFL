(**
    Abstract syntax trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


(** {2 Types} *)

type id = Ident.t
(** Identifiers for variables, function arguments, and bindings. *)

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list
(** The type of literal values. *)

and exp =
  | Literal of lit
  | Variable of id
  | Application of exp * exp
  | Abstraction of id * Type.t * exp
  | Binding of id * Type.t * exp * exp
(** Abstract syntax tree expressions.  *)

val top_tag : exp
(** A tag for top-level bindings. *)


(** {2 Type functions} *)

val to_type : Type.t Ident.Map.t -> exp -> Type.t
(**
  [to_type env exp] computes the type of [exp] under the environment
  [env], assuming all function applications and identifier bindings are
  well-typed (i.e. the actual type of the argument or bound-value
  matches it's expected type).
*)

val to_type_list : Type.t Ident.Map.t -> exp list -> Type.t list
(**
  As [to_type], except that [to_type_list] expects a list of expressions
  and returns a list of their types.
*)

val to_ident_list : exp list -> id list
(**
  As [to_type], except that [to_type_list] expects a list of expressions
  and returns a list of their types.
*)

val typecheck : Type.t Ident.Map.t -> exp -> Type.t
(**
  [typecheck env exp] computes the type of [exp] under the environment
  [env], or throws an exception if [exp] is not well-typed.
*)

val constrain : exp list -> (Type.t * Type.t) list
(**
  [constrain es] computes the type constraints that would need to be
  satisfied for [es] to be well-typed.
*)


(** {2 String functions} *)

val lit_to_string : lit -> string
(**
  [lit_to_string lit] computes the string representation for the literal
  [lit].
*)

val exp_to_string : exp -> string
(**
  [exp_to_string exp] computes the string representation for the
  expression [exp].
*)
