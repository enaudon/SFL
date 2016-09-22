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

module TT = Type_tree
module AST = Abs_syntax_tree
module AST2 = Abs_syntax_tree2

let rec type_t_of_tt_type tp = match tp with
  | TT.Boolean -> Type.Boolean
  | TT.Integer -> Type.Integer
  | TT.Variable tv ->
    let tv' = Type.TypeVariable.create (TT.TypeVariable.name tv) in
    Type.Variable tv'
  | TT.Function (arg, ret) ->
    let arg' = type_t_of_tt_type arg in
    let ret' = type_t_of_tt_type ret in
    Type.Function (arg', ret')
  | TT.Product tps ->
    let tps' = List.map type_t_of_tt_type tps in
    Type.Tuple tps'
  | TT.Disjunction _ -> failwith "TT.Disjunction not implemented"
let lit_of_tt l = match l with
  | AST.Integer i -> Integer i
  | AST.Boolean b -> Boolean b
  | AST.Tuple _ -> failwith "AST.Tuple to IR not implemented"

let rec binop_of_tt_abs (exp1, exp2) = match exp1, exp2 with
  | AST.Variable (id, _), AST.Literal (AST.Tuple [lhs; rhs], _) ->
    let op = binop_of_string id in
    let lhs' = exp_of_tt lhs in
    let rhs' = exp_of_tt rhs in
    (op, lhs', rhs')
  | _ ->
    failwith "IR.binop_of_tt_abs: AST.Application to IR not implemented"

and exp_of_tt exp = match exp with
  | AST.Variable (id, _) -> Variable id
  | AST.Literal (l, _) -> Literal (lit_of_tt l)
  | AST.Application (exp1, exp2, _) ->
    begin try
      let op, lhs, rhs = binop_of_tt_abs (exp1, exp2) in
      BinaryOperation (op, lhs, rhs)
    with _ -> match exp1 with
      | AST.Variable (id, _) ->
        let args = [ exp_of_tt exp2 ] in
        Application(id, args)
      | _ -> failwith "IR.exp_of_tt: AST.Application unrecognized function"
    end
  | AST.Abstraction (_, _, _) ->
    failwith "IR.exp_of_tt: AST.Abstraction to IR not implemented"
  | AST.Binding (id, value, exp, _) ->
    let value' = exp_of_tt value in
    let exp' = exp_of_tt exp in
    Binding (id, value', exp')

let top_of_tt exp = match exp with
  | AST.Binding (id, AST.Abstraction (arg, body, _), AST.Variable ("", _), tp) ->
    let body' = exp_of_tt body in
    FunctionDecl (id, [arg], body', type_t_of_tt_type tp)
  | AST.Binding (id, value, AST.Variable ("", _), _) ->
    let value' = exp_of_tt value in
    VariableDecl (id, value')
  | _ -> Expression (exp_of_tt exp)

let lit_of_ast l = match l with
  | AST2.Integer i -> Integer i
  | AST2.Boolean b -> Boolean b
  | AST2.Tuple _ -> failwith "IR.exp_of_tt: AST.Tuple to IR not implemented"

let exp_of_ast_app id args = match id with
  | "+" | "-" | "*" | "/" ->
    let op = binop_of_string id in
    begin match args with
      | [rhs; lhs] -> BinaryOperation (op, rhs, lhs)
      | _ -> failwith "IR.exp_of_ast_app: invalid arguments to binop"
    end
  | _ -> Application (id, args)

let rec exp_of_ast exp = match exp with
  | AST2.Variable id -> Variable id
  | AST2.Literal l -> Literal (lit_of_ast l)
  | AST2.Application _ ->
    let rec helper exp = match exp with
      | AST2.Application (fn, arg) -> arg :: helper fn
      | _ -> [exp]
    in
    let id, args = match List.rev (helper exp) with
      | AST2.Variable (id) :: args -> (id, List.map exp_of_ast args)
      | _ -> failwith "IR.exp_of_ast: unsupported application of non-ID"
    in
    exp_of_ast_app id args
  | AST2.Abstraction _ ->
    failwith "IR.exp_of_ast: AST.Abstraction to IR not implemented"
  | AST2.Binding (id, _, value, exp) ->
    let value' = exp_of_ast value in
    let exp' = exp_of_ast exp in
    Binding (id, value', exp')

let top_of_ast exp = match exp with
  | AST2.Binding (id, tp, value, exp) when exp = AST2.top_tag ->
    begin match value with
      | AST2.Abstraction (arg, _, body) ->
        let body' = exp_of_ast body in
        FunctionDecl (id, [arg], body', tp)
      | _ ->
        let value' = exp_of_ast value in
        VariableDecl (id, value')
    end
  | _ -> Expression (exp_of_ast exp)
