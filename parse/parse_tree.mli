(**
    Parse trees and associated functions.

    The parse tree is a faithful tree-form representation of the source
    code.  No desugaring has been done, except that parenthesis are
    implicit. Types have not yet been infered.
 *)


(** {2 Types} *)

(** Variable names. *)
type id = string

(** The type of binary operations *)
type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

(** The type of literal values. *)
type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

(** The type of parse tree expressions. *)
and exp =
  | Variable of id
  | Literal of lit
  | BinaryOperation of binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp

(** The type of the parse tree top level. *)
type top =
  | Declaration of exp * exp
  | Expression of exp

(** {2 Functions} *)

(** Translate an expression into an AST expression. *)
val exp_to_ast : exp -> unit Abs_syntax_tree.exp

(** Translate an top-level expression into an AST expression. *)
val top_to_ast : top -> unit Abs_syntax_tree.exp

(** Returns the string representation of a binary operator. *)
val binop_to_string : binop -> string

(** Returns the string representation of an expression. *)
val exp_to_string : exp -> string

(** Returns the string representation of a top-level expression. *)
val top_to_string : top -> string
