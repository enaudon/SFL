module AST = Abs_syntax_tree
module PT = Parse_tree
module Env = Ident.Map

let rec lit_to_ast env lit = match lit.PT.lit_desc with
  | PT.Boolean b -> AST.Boolean b
  | PT.Integer i -> AST.Integer i
  | PT.Tuple es ->
    let es' = List.map (exp_to_ast env) es in
    AST.Tuple es'

and exp_to_ast env exp = match exp.PT.exp_desc with
  | PT.Variable id -> AST.Variable (Ident.of_string id)
  | PT.Literal l ->
    let l' = lit_to_ast env l in
    AST.Literal (l')
  | PT.BinaryOperation (op, e1, e2) ->
    exp_to_ast env {
      PT.exp_desc = PT.Application (
        {
          PT.exp_desc = PT.Application (
            {
              PT.exp_desc = PT.Variable (Primative.binop_to_string op) ;
              PT.exp_pos =
                Position.create
                  (Position.file exp.PT.exp_pos)
                  (Position.start exp.PT.exp_pos)
                  (Position.start e1.PT.exp_pos) ;
            },
            e1
          ) ;
          PT.exp_pos =
            Position.create
              (Position.file exp.PT.exp_pos)
              (Position.start exp.PT.exp_pos)
              (Position.finish e1.PT.exp_pos) ;
        },
        e2 ;
      ) ;
      PT.exp_pos = exp.PT.exp_pos ;
    }
  | PT.Application (fn, arg) ->
    let fn' = exp_to_ast env fn in
    let arg' = exp_to_ast env arg in
    AST.Application (fn', arg')
  | PT.Abstraction (arg, body) ->
    let arg' = Ident.of_string arg in
    let body' = exp_to_ast env body in
    let tp = Type.Variable (Type.TypeVariable.create arg) in
    AST.Abstraction (arg', tp, body')
  | PT.Binding (binds, body) ->
    let rec fn env binds = match binds with
      | [] -> exp_to_ast env body
      | (id, value) :: tl ->
        let id' = Ident.of_string id in
        let value' = exp_to_ast env value in
        let tp = AST.to_type env value' in
        let env' = Env.add id' tp env in
        let exp = fn env' tl in
        AST.Binding (id', tp, value', exp)
    in
    fn env binds

let rec top_to_ast env exps = match exps with
  | [] -> []
  | hd :: tl ->
    match hd.PT.top_desc with
      | PT.Declaration (lhs, rhs) -> begin match lhs.PT.exp_desc with
        | PT.Variable id ->
          let id' = Ident.of_string id in
          let value = exp_to_ast env rhs in
          let tp = AST.to_type env value in
          let env' = Env.add id' tp env in
          AST.Binding (id', tp, value, AST.top_tag) :: top_to_ast env' tl
        | PT.Application (
          { PT.exp_desc = PT.Variable id; _ },
          { PT.exp_desc = PT.Variable arg; _ }
        ) ->
          let id' = Ident.of_string id in
          let arg' = Ident.of_string arg in
          let body = exp_to_ast env rhs in
          let arg_tp = Type.Variable (Type.TypeVariable.create arg) in
          let ret_tp = AST.to_type (Env.add arg' arg_tp env) body in
          let fn_tp = Type.Function (arg_tp, ret_tp) in
          let fn = AST.Abstraction (arg', arg_tp, body) in
          let env' = Env.add id' fn_tp env in
          AST.Binding (id', fn_tp, fn, AST.top_tag)
          :: top_to_ast env' tl
        | _ ->
          failwith (
            Printf.sprintf
              "%s: expected a variable or function declaration"
              (Position.to_string lhs.PT.exp_pos)
          )
      end
      | PT.Expression exp ->
        exp_to_ast env exp :: top_to_ast env tl

let f = top_to_ast Primative.tp_env
