module AST = Abs_syntax_tree
module IR = Internal_rep


let lit_of_ast l = match l.AST.lit_desc with
  | AST.Integer i -> IR.Integer i
  | AST.Boolean b -> IR.Boolean b
  | AST.Tuple _ -> failwith "IR.exp_of_tt: AST.Tuple to IR not implemented"

let exp_of_ast_app id args =
  try
    let op = Primative.binop_of_string (Ident.to_string id) in
    match args with
      | [rhs; lhs] -> IR.BinaryOperation (op, rhs, lhs)
      | _ -> failwith "IR.exp_of_ast_app: invalid arguments to binop"
  with
    | _ -> IR.Application (id, args)

let rec exp_of_ast exp = match exp.AST.exp_desc with
  | AST.Variable id -> IR.Variable id
  | AST.Literal l -> IR.Literal (lit_of_ast l)
  | AST.Application _ ->
    let rec helper exp = match exp.AST.exp_desc with
      | AST.Application (fn, arg) -> arg :: helper fn
      | _ -> [exp]
    in
    let id, args = match List.rev (helper exp) with
      | { AST.exp_desc = AST.Variable (id) ; _ } :: args ->
        (id, List.map exp_of_ast args)
      | _ -> failwith "IR.exp_of_ast: unsupported application of non-ID"
    in
    exp_of_ast_app id args
  | AST.Abstraction _ ->
    failwith "IR.exp_of_ast: AST.Abstraction to IR not implemented"
  | AST.Binding (id, _, value, exp) ->
    let value' = exp_of_ast value in
    let exp' = exp_of_ast exp in
    IR.Binding (id, value', exp')

let top_of_ast exp = match exp.AST.exp_desc with
  | AST.Binding (id, tp, value, exp) when exp = AST.top_tag ->
    begin match value.AST.exp_desc with
      | AST.Abstraction (arg, _, body) ->
        let body' = exp_of_ast body in
        IR.FunctionDecl (id, [arg], body', tp)
      | _ ->
        let value' = exp_of_ast value in
        IR.VariableDecl (id, value')
    end
  | _ -> IR.Expression (exp_of_ast exp)

let f = top_of_ast
