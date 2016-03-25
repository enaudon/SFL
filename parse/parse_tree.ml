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

type literal =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

and exp =
  | Variable of id
  | Literal of literal
  | BinaryOperation of binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Declaration of exp * exp * exp

type top =
  | VariableDecl of id * exp
  | FunctionDecl of id * id list * exp
  | Expression of exp

let binop_to_string op = match op with
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
  | Modulo -> "%"

let rec literal_to_string (l : literal) : string = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map to_string es))

and to_string pt =
  (* Returns the parenthesized string representation of a parse tree *)
  let paren pt = match pt with
    | Variable x -> x
    | Literal l -> literal_to_string l
    | _ -> Printf.sprintf "(%s)" (to_string pt)
  in
  match pt with
    | Variable x -> x
    | Literal l -> literal_to_string l
    | BinaryOperation (op, pt1, pt2) ->
      Printf.sprintf "%s %s %s"
        (to_string pt1)
        (binop_to_string op)
        (to_string pt2)
    | Abstraction (x, pt) ->
      Printf.sprintf "%s -> %s" x (paren pt)
    | Application (pt1, pt2) ->
      Printf.sprintf "%s %s" (paren pt1) (paren pt2)
    | Declaration (pt1, pt2, pt3) ->
      Printf.sprintf "let %s = %s in %s end"
        (to_string pt1)
        (to_string pt2)
        (to_string pt3)

let top_to_string top = match top with
  | VariableDecl (id, exp) ->
    Printf.sprintf "%s = %s\n" id (to_string exp)
  | FunctionDecl (id, args, body) ->
    let args_str = String.concat ", " args in
    Printf.sprintf "%s(%s) = %s\n" id args_str (to_string body)
  | Expression exp ->
    Printf.sprintf "%s\n" (to_string exp)
