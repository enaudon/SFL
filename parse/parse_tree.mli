(**
    Parse trees and associated functions.

    The parse tree is a faithful tree-form representation of the source
    code.  No desugaring has been done, except that parenthesis are
    implicit. Types have not yet been infered.
 *)


(** {2 Types} *)

(** Variable names. *)
type id = string

(** The type of literal values. *)
type literal =
  | Boolean of bool
  | Integer of int

(** The type of parse trees. *)
type t =
  | Variable of id
  | Literal of literal
  | Abstraction of id * t
  | Application of t * t
  | Declaration of t * t * t


(** {2 Functions} *)

(** Returns the string representation of a parse tree. *)
val to_string : t -> string
