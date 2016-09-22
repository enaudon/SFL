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

let binop_of_string id = match id with
  | "+" -> Addition
  | "-" -> Subtraction
  | "*" -> Multiplication
  | "/" -> Division
  | "%" -> Modulo
  | _ -> failwith "unsupported binary operation"

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

module AST = Abs_syntax_tree

let lit_of_ast l = match l with
  | AST.Integer i -> Integer i
  | AST.Boolean b -> Boolean b
  | AST.Tuple _ -> failwith "IR.exp_of_tt: AST.Tuple to IR not implemented"

let exp_of_ast_app id args = match id with
  | "+" | "-" | "*" | "/" ->
    let op = binop_of_string id in
    begin match args with
      | [rhs; lhs] -> BinaryOperation (op, rhs, lhs)
      | _ -> failwith "IR.exp_of_ast_app: invalid arguments to binop"
    end
  | _ -> Application (id, args)

let rec exp_of_ast exp = match exp with
  | AST.Variable id -> Variable id
  | AST.Literal l -> Literal (lit_of_ast l)
  | AST.Application _ ->
    let rec helper exp = match exp with
      | AST.Application (fn, arg) -> arg :: helper fn
      | _ -> [exp]
    in
    let id, args = match List.rev (helper exp) with
      | AST.Variable (id) :: args -> (id, List.map exp_of_ast args)
      | _ -> failwith "IR.exp_of_ast: unsupported application of non-ID"
    in
    exp_of_ast_app id args
  | AST.Abstraction _ ->
    failwith "IR.exp_of_ast: AST.Abstraction to IR not implemented"
  | AST.Binding (id, _, value, exp) ->
    let value' = exp_of_ast value in
    let exp' = exp_of_ast exp in
    Binding (id, value', exp')

let top_of_ast exp = match exp with
  | AST.Binding (id, tp, value, exp) when exp = AST.top_tag ->
    begin match value with
      | AST.Abstraction (arg, _, body) ->
        let body' = exp_of_ast body in
        FunctionDecl (id, [arg], body', tp)
      | _ ->
        let value' = exp_of_ast value in
        VariableDecl (id, value')
    end
  | _ -> Expression (exp_of_ast exp)
