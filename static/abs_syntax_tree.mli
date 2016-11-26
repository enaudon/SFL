(**
    Abstract syntax trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


type id = Ident.t
(** Identifiers for variables, function arguments, and bindings. *)

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list
(** The type of literal values. *)

and exp =
  | Variable of id
  | Literal of lit
  | Application of exp * exp
  | Abstraction of id * Type.t * exp
  | Binding of id * Type.t * exp * exp
(** The type of abstract syntax tree expressions.  *)

val top_tag : exp
(** A tag for top-level bindings. *)

val to_type : Type.t Ident.Map.t -> exp -> Type.t
(**
  [to_type env exp] computes the type of [exp] under the environment
  [env], assuming all function applications are well-typed (i.e. the
  actual argument type matches the expected type).
*)

val typecheck : Type.t Ident.Map.t -> exp -> Type.t
(**
  [typecheck env exp] computes the type of [exp] under the environment
  [env], or throws an exception if [exp] is not well-typed.
*)

(** Type checks an expression. *)
val constrain : exp list -> (Type.t * Type.t) list

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
