module AST = Abs_syntax_tree
module PT = Parse_tree
module Env = AST.Env

let rec lit_to_ast env lit = match lit with
  | PT.Boolean b -> AST.Boolean b
  | PT.Integer i -> AST.Integer i
  | PT.Tuple es ->
    let es' = List.map (exp_to_ast env) es in
    AST.Tuple es'

and exp_to_ast env exp = match exp with
  | PT.Variable id -> AST.Variable (id)
  | PT.Literal l ->
    let l' = lit_to_ast env l in
    AST.Literal (l')
  | PT.BinaryOperation (op, e1, e2) ->
    exp_to_ast env (
      PT.Application (
        PT.Application (PT.Variable (PT.binop_to_string op), e1),
        e2
      )
    )
  | PT.Application (fn, arg) ->
    let fn' = exp_to_ast env fn in
    let arg' = exp_to_ast env arg in
    AST.Application (fn', arg')
  | PT.Abstraction (arg, body) ->
    let body' = exp_to_ast env body in
    let tp = Type.Variable (Type.TypeVariable.create arg) in
    AST.Abstraction (arg, tp, body')
  | PT.Binding (binds, body) ->
    let rec fn env binds = match binds with
      | [] -> exp_to_ast env body
      | (id, value) :: tl ->
        let value' = exp_to_ast env value in
        let tp = AST.to_type env value' in
        let env' = Env.add id tp env in
        let exp = fn env' tl in
        AST.Binding (id, tp, value', exp)
    in
    fn env binds

let rec top_to_ast env exps = match exps with
  | [] -> []
  | hd :: tl ->
    match hd with
      | PT.Declaration (lhs, rhs) -> begin match lhs with
        | PT.Variable id ->
          let value = exp_to_ast env rhs in
          let tp = AST.to_type env value in
          let env' = Env.add id tp env in
          AST.Binding (id, tp, value, AST.top_tag) :: top_to_ast env' tl
        | PT.Application (PT.Variable id, PT.Variable arg) ->
          let body = exp_to_ast env rhs in
          let arg_tp = Type.Variable (Type.TypeVariable.create arg) in
          let ret_tp = AST.to_type (Env.add arg arg_tp env) body in
          let fn_tp = Type.Function (arg_tp, ret_tp) in
          let fn = AST.Abstraction (arg, arg_tp, body) in
          let env' = Env.add id fn_tp env in
          AST.Binding (id, fn_tp, fn, AST.top_tag) :: top_to_ast env' tl
        | _ ->
          failwith "Expected a variable or function declaration"
      end
      | PT.Expression exp ->
        exp_to_ast env exp :: top_to_ast env tl

let f = top_to_ast AST.empty_env
