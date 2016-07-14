module AST = Abs_syntax_tree
module StrMap = Map.Make (String)

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
  | Application of id * exp
  | Binding of (id * exp) list * exp

type top =
  | VariableDecl of id * exp
  | FunctionDecl of id * id list * exp * Type_tree.typo
  | Expression of exp

let lit_of_tt l = match l with
  | AST.Integer i -> Integer i
  | AST.Boolean b -> Boolean b
  | AST.Tuple _ -> failwith "AST.Tuple to IR not implemented"

let binop_of_string id = match id with
  | "+" -> Addition
  | "-" -> Subtraction
  | "*" -> Multiplication
  | "/" -> Division
  | "%" -> Modulo
  | _ -> failwith "unsupported binary operation"

let rec binop_of_tt_abs (exp1, exp2) = match exp1, exp2 with
  | AST.Variable (id, _), AST.Literal (AST.Tuple [lhs; rhs], _) ->
    let op = binop_of_string id in
    let lhs' = exp_of_tt lhs in
    let rhs' = exp_of_tt rhs in
    (op, lhs', rhs')
  | _ ->
    failwith "AST.Application to IR not implemented"

and exp_of_tt tt = match tt with
  | AST.Variable (id, _) -> Variable id
  | AST.Literal (l, _) -> Literal (lit_of_tt l)
  | AST.Application (exp1, exp2, _) ->
    let op, lhs, rhs = binop_of_tt_abs (exp1, exp2) in
    BinaryOperation (op, lhs, rhs)
  | AST.Abstraction (_, _, _) ->
    failwith "AST.Application to IR not implemented"
  | AST.Binding (binds, exp, _) ->
    let bind_fn (id, e) = (id, (exp_of_tt e)) in
    let binds' = List.map bind_fn binds in
    let exp' = exp_of_tt exp in
    Binding (binds', exp')

let top_of_tt tt =
  let fn tt = match tt with
    | AST.VariableDecl (id, exp, _) ->
      let exp' = exp_of_tt exp in
      VariableDecl (id, exp')
    | AST.FunctionDecl (id, args, body, tp) ->
      let body' = exp_of_tt body in
      FunctionDecl (id, args, body', tp)
    | AST.Expression (exp, _) ->
      let exp' = exp_of_tt exp in
      Expression exp'
  in
  fn tt

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
  | Application (id, exp) ->
    Printf.sprintf "%s(%s)\n"
      id
      (exp_to_string exp)
  | Binding (binds, exp) ->
    let fn (id, e) = Printf.sprintf "%s = %s" id (exp_to_string e) in
    Printf.sprintf "let %s in %s"
      (String.concat "; " (List.map fn binds))
      (exp_to_string exp)

let top_to_string top = match top with
  | VariableDecl (id, exp) ->
    Printf.sprintf "%s = %s\n"
      id
      (exp_to_string exp)
  | FunctionDecl (id, args, body, _) ->
    Printf.sprintf "%s(%s) = %s\n"
      id
      (String.concat ", " args)
      (exp_to_string body)
  | Expression (exp) ->
    Printf.sprintf "%s\n" (exp_to_string exp)
