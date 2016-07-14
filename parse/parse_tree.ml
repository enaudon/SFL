(*
 * Parse Tree
 *)

type id = string

type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

and exp =
  | Variable of id
  | Literal of lit
  | BinaryOperation of binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp

type top =
  | Declaration of exp * exp
  | Expression of exp

let binop_to_string op = match op with
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
  | Modulo -> "%"

let rec lit_to_string (l : lit) : string = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string pt =
  (* Returns the parenthesized string representation of a parse tree *)
  let paren pt = match pt with
    | Variable x -> x
    | Literal l -> lit_to_string l
    | _ -> Printf.sprintf "(%s)" (exp_to_string pt)
  in
  match pt with
    | Variable x -> x
    | Literal l -> lit_to_string l
    | BinaryOperation (op, pt1, pt2) ->
      Printf.sprintf "(%s %s %s)"
        (exp_to_string pt1)
        (binop_to_string op)
        (exp_to_string pt2)
    | Application (pt1, pt2) ->
      Printf.sprintf "%s(%s)" (paren pt1) (exp_to_string pt2)
    | Abstraction (arg, body) ->
      Printf.sprintf "(%s -> %s)" arg (exp_to_string body)
    | Binding (binds, pt) ->
      let fn (id, e) = Printf.sprintf "%s = %s" id (exp_to_string e) in
      Printf.sprintf "let %s in %s"
        (String.concat "; " (List.map fn binds))
        (exp_to_string pt)

let top_to_string top = match top with
  | Declaration (exp1, exp2) ->
    Printf.sprintf "%s = %s\n" (exp_to_string exp1) (exp_to_string exp2)
  | Expression exp ->
    Printf.sprintf "%s\n" (exp_to_string exp)
