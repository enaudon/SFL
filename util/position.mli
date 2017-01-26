(**
 *  Information on positions within the source code.
 *
 *  This module encapsulates the notion of a position within the source
 *  code.  It is useful for localizing messages about the code.
 **)

(** The type for a location within the source code *)
type t

(**
  [create file (sr, sc) (er, ec)] creates a new location from the
  filename [file], start row and column [sr] and [sc], and end row and
  column [er] and [ec].
 **)
val create : string -> (int * int) -> (int * int) -> t

(** [file loc] computes the file from a the location [loc]. *)
val file : t -> string

(** [start loc] computes the start row and column from a the location
  [loc].
 **)
val start : t -> int * int

(**
  [finish loc] computes the finish from a the location [loc].
 **)
val finish : t -> int * int
