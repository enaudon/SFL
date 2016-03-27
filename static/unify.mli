(**  Unification *)


module Identifier : sig
  type t = int

  val reset : unit -> unit
  val fresh : unit -> t

  module type SET = Set.S with type elt = t
  module Set : SET

  module type MAP = Map.S with type key = t
  module Map : MAP
end

type term =
  | Variable of Identifier.t
  | Function of Identifier.t * term array
  | Disjunction of term list

module Substitution : sig
  type t = term Identifier.Map.t

  val identity : t
  val singleton : Identifier.t -> term -> t

  val apply : t -> term -> term
  val compose : t -> t -> t
end

val variable : Identifier.t -> term
val constant : Identifier.t -> term
val funktion : Identifier.t * term array -> term

val term_to_string : term -> string
val constraint_to_string : term * term -> string

val unify : (term * term) list -> Substitution.t
