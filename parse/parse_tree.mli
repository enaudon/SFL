(**
    Parse trees and associated functions.

    The parse tree is a faithful tree-form representation of the source
    code.  No desugaring has been done, except that parenthesis are
    implicit. Types have not yet been infered.
 *)


(** {2 Types} *)

type id = string
(** Identifiers for variables, function arguments, and bindings. *)

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list
(** Literal values. *)

and exp =
  | Variable of id
  | Literal of lit
  | BinaryOperation of Primative.binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp
(** Parse tree expressions. *)

type top =
  | Declaration of exp * exp
  | Expression of exp
(** Parse tree top-level statements. *)


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

val top_to_string : top -> string
(**
  [top_to_string top] computes the string representation for the
  top-level statement [top].
*)
