(**
    Parse trees and associated functions.

    The parse tree is a faithful tree-form representation of the source
    code.  No desugaring has been done, except that parenthesis are
    implicit. Types have not yet been infered.
 **)


(** {2 Types} *)

type id = string
(** Identifiers for variables, function arguments, and bindings. *)

type pos =
  | Prefix
  | Postfix
(** The position of the function, for function application **)

type lit_desc =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list
(** Literal values. *)

and lit = {
  lit_desc : lit_desc ;
  lit_pos : Position.t ;
}
(** Literals, along with meta-data. *)

and exp_desc =
  | Variable of id
  | Literal of lit
  | Application of exp * exp * pos
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp
(** Parse tree expressions. *)

and exp = {
  exp_desc : exp_desc ;
  exp_pos : Position.t ;
}
(** Expressions, along with meta-data. *)

type top_desc =
  | Declaration of exp * exp
  | Expression of exp
(** Top-level expressions. *)

type top = {
  top_desc : top_desc ;
  top_pos : Position.t ;
}
(** Top-level expressions, along with meta-data. *)


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
