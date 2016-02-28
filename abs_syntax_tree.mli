(**
    Abstract syntax trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


(** Variable names. *)
type id = string

(** The type of literal values. *)
type literal =
  | Boolean of bool
  | Integer of int

(** The type of abstract syntax trees of type ['a].  An abstact syntax
    tree carries data at each node.
 *)
type 'a t =
  | Variable of id * 'a
  | Literal of literal * 'a
  | Abstraction of id * 'a t * 'a
  | Application of 'a t * 'a t * 'a
  | Declaration of id * 'a t * 'a t * 'a

(** Returns the data associated with an abstract syntax tree. *)
val data : 'a t -> 'a

(** [map fn ast] returns a new abstract syntax tree with the same
    structure as [ast], but where the data, [d], at with each node has
    been replaced with the result of applying [fn] to [d].
 *)
val map : ('a -> 'b) -> 'a t -> 'b t
