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

type exp =
  | Literal of lit
  | Variable of id
  | BinaryOperation of binop * exp * exp
  | Application of id * exp list
  | Binding of id * exp * exp

type top =
  | VariableDecl of id * exp
  | FunctionDecl of id * id list * exp * Type.t
  | Expression of exp

let lit_to_string l = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i

let binop_to_string op = match op with
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
  | Modulo -> "%"

let rec exp_to_string exp = match exp with
  | Literal l -> lit_to_string l
  | Variable id -> id
  | BinaryOperation (op, lhs, rhs) ->
    Printf.sprintf "%s %s %s"
      (exp_to_string lhs)
      (binop_to_string op)
      (exp_to_string rhs)
  | Application (id, args) ->
    let args' = List.map exp_to_string args in
    Printf.sprintf "%s(%s)"
      id
      (String.concat ", " args')
  | Binding (id, value, exp) ->
    Printf.sprintf "let %s = %s in %s"
      id
      (exp_to_string value)
      (exp_to_string exp)

let top_to_string top = match top with
  | VariableDecl (id, exp) ->
    Printf.sprintf "%s = %s\n"
      id
      (exp_to_string exp)
  | FunctionDecl (id, args, body, tp) ->
    Printf.sprintf "%s : %s (%s) = %s\n"
      id
      (Type.to_string tp)
      (String.concat ", " args)
      (exp_to_string body)
  | Expression (exp) ->
    Printf.sprintf "%s\n" (exp_to_string exp)
