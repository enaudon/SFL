(**
    Information on positions within the source code.

    This module encapsulates the notion of a position within the source
    code.  It is useful for localizing messages about the code.
 **)

(** The type for a position within the source code *)
type t

(**
  [create file (sr, sc) (er, ec)] creates a new position from the
  filename [file], start row and column [sr] and [sc], and end row and
  column [er] and [ec].
 **)
val create : string -> (int * int) -> (int * int) -> t

(** [dummy] is an invalid, dummy position.  **)
val dummy : t

(** [file pos] computes the file from a the position [pos]. *)
val file : t -> string

(**
  [start pos] computes the start row and column from a the position
  [pos].
 **)
val start : t -> int * int

(**
  [finish pos] computes the finish from a the position [pos].
 **)
val finish : t -> int * int

(**
  [to_string pos] computes a string representation of the position
  [pos].
**)
val to_string : t -> string
