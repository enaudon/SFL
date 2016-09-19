(**
    Abstract syntax trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


(** Variable names. *)
type id = string

(** The type of literal values. *)
type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

(** The type of abstract syntax trees of type ['a].  An abstact syntax
    tree carries data at each node.
 *)
and exp =
  | Variable of id
  | Literal of lit
  | Application of exp * exp
  | Abstraction of id * Ast_type.t * exp
  | Binding of id * Ast_type.t * exp * exp

(** Creates a tag expression for top-level bindings *)
val top_tag : exp

(** Computes the type of an expression *)
val to_type : exp -> Ast_type.t

(** Type checks an expression *)
val typecheck : exp -> Ast_type.t

val constrain : exp list -> (Ast_type.t * Ast_type.t) list

val lit_to_string : lit -> string
val exp_to_string : exp -> string
