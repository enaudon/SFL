type t =
  | Boolean of bool
  | Integer of int
  | Function of (t -> t)

let format ff v = match v with
  | Boolean b -> Format.fprintf ff "%b" b
  | Integer i -> Format.fprintf ff "%i" i
  | Function _ -> Format.fprintf ff "<function>"

let to_string = Format.asprintf "%a" format
