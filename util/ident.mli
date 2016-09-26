type t
val compare : t -> t -> int


(** {2 String Functions} *)

(* Compute an identifier from a string. *)
val of_string : string -> t

(* Compute a string from an identifier.*)
val to_string : t -> string


(** {2 Containers} *)

(** Sets of identifiers. *)
module type SET = Set.S with type elt = t
module Set : SET

(** Maps of identifiers. *)
module type MAP = Map.S with type key = t
module Map : MAP
