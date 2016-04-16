(**
    Type trees and associated functions.

    The type tree is a representation of the syntactic structure of the
    source code, with the addition of type information.
 *)


(** {2 Type Variables} *)

(** Globaly unique identifiers for type variables. *)
module TypeVariable : sig
  (** The type of type variables *)
  type t

  (** Reset the global variable counter. *)
  val reset : unit -> unit

  (** Creates a new type variable. *)
  val create : string -> t

  (** Returns the string representation of a type variable. *)
  val to_string : t -> string


  (** {2 Containers} *)

  (** The type of sets of type variables. *)
  module type SET = Set.S with type elt = t

  module Set : SET
  (** Sets of type variables. *)

  (** The type of maps of type variables. *)
  module type MAP = Map.S with type key = t

  (** Maps of type variables. *)
  module Map : MAP

end

(** The type of types. *)
type typo =
  | Boolean
  | Integer
  | Variable of TypeVariable.t
  | Function of typo * typo
  | Product of typo list
  | Disjunction of typo list

(** The type of type tree expressions. *)
type exp = typo Abs_syntax_tree.exp

(** The type of type trees. *)
type top = typo Abs_syntax_tree.top


(** {2 Functions} *)

(** Returns the type of a type tree *)
val to_typo : top -> typo

(** Returns the string representation of a type tree type. *)
val typo_to_string : typo -> string

(** Returns the string representation of a type tree expression. *)
val exp_to_string : exp -> string

(** Returns the string representation of a type tree. *)
val top_to_string : top -> string
