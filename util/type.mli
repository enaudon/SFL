(**
 *  Types.
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

  (** Returns the name of a type variable. *)
  val name : t -> string


  (** {2 Containers} *)

  (** The type of sets of type variables. *)
  module type SET = Set.S with type elt = t

  (** Sets of type variables. *)
  module Set : SET

  (** The type of maps of type variables. *)
  module type MAP = Map.S with type key = t

  (** Maps of type variables. *)
  module Map : MAP

end

(** The type of types. *)
type t =
  | Boolean
  | Integer
  | Variable of TypeVariable.t
  | Function of t * t
  | Tuple of t list

(** Returns the string representation of a type. *)
val to_string : t -> string
