(**
    Abstract syntax trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


(** Variable names. *)
type id = string

(** The type of literal values. *)
type 'a literal =
  | Boolean of bool
  | Integer of int
  | Tuple of 'a exp list

(** The type of abstract syntax trees of type ['a].  An abstact syntax
    tree carries data at each node.
 *)
and 'a exp =
  | Variable of id * 'a
  | Literal of 'a literal * 'a
  | Application of 'a exp * 'a exp * 'a
  | Binding of (id * 'a exp) list * 'a exp * 'a

type 'a top =
  | VariableDecl of id * 'a exp * 'a
  | FunctionDecl of id * id list * 'a exp * 'a
  | Expression of 'a exp * 'a

(** Returns the data associated with an abstract syntax tree expression.
 *)
val exp_data : 'a exp -> 'a

(** Returns the data associated with an abstract syntax tree expression.
 *)
val top_data : 'a top -> 'a

(** [map fn ast] returns a new abstract syntax tree with the same
    structure as [ast], but where the data, [d], at with each node has
    been replaced with the result of applying [fn] to [d].
 *)
val map : ('a -> 'b) -> 'a top -> 'b top

val lit_to_string : 'a literal -> string
val exp_to_string : 'a exp -> string
val top_to_string : 'a top -> string
