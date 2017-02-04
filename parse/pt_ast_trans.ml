module AST = Abs_syntax_tree
module PT = Parse_tree
module Env = Ident.Map

let rec lit_to_ast env lit =
  let lit_desc = match lit.PT.lit_desc with
    | PT.Boolean b -> AST.Boolean b
    | PT.Integer i -> AST.Integer i
    | PT.Tuple es ->
      let es' = List.map (exp_to_ast env) es in
      AST.Tuple es'
  in
  {
    AST.lit_desc ;
    AST.lit_pos = lit.PT.lit_pos ;
  }

(*
  TODO: pull the record stuff out of the match.
  Once you remove binary operations from the PT and extend the AST to
  support bindings with multiple id/val pairs, you should be able to do
  that with no problem.
 *)
and exp_to_ast env exp = match exp.PT.exp_desc with
  | PT.Variable id ->
    {
      AST.exp_desc = AST.Variable (Ident.of_string id) ;
      AST.exp_pos = exp.PT.exp_pos ;
    }
  | PT.Literal l ->
    let l' = lit_to_ast env l in
    {
      AST.exp_desc = AST.Literal l' ;
      AST.exp_pos = exp.PT.exp_pos ;
    }
  | PT.Application (fn, arg, _) ->
    let fn' = exp_to_ast env fn in
    let arg' = exp_to_ast env arg in
    {
      AST.exp_desc = AST.Application (fn', arg') ;
      AST.exp_pos = exp.PT.exp_pos ;
    }
  | PT.Abstraction (arg, body) ->
    let arg' = Ident.of_string arg in
    let body' = exp_to_ast env body in
    let tp = Type.Variable (Type.TypeVariable.create arg) in
    {
      AST.exp_desc = AST.Abstraction (arg', tp, body') ;
      AST.exp_pos = exp.PT.exp_pos ;
    }
  | PT.Binding (binds, body) ->
    let rec fn env binds = match binds with
      | [] -> exp_to_ast env body
      | (id, value) :: tl ->
        let id' = Ident.of_string id in
        let value' = exp_to_ast env value in
        let tp = AST.to_type env value' in
        let env' = Env.add id' tp env in
        let exp = fn env' tl in
        {
          AST.exp_desc = AST.Binding (id', tp, value', exp) ;
          AST.exp_pos = value.PT.exp_pos ;
        }
    in
    fn env binds

let rec top_to_ast env exps = match exps with
  | [] -> []
  | hd :: tl -> match hd.PT.top_desc with
    | PT.Declaration (lhs, rhs) ->
      let env', exp_desc = match lhs.PT.exp_desc with
        | PT.Variable id ->
          let id' = Ident.of_string id in
          let value = exp_to_ast env rhs in
          let tp = AST.to_type env value in
          let env' = Env.add id' tp env in
          let desc = AST.Binding (id', tp, value, AST.top_tag) in
          env', desc
        | PT.Application (
          { PT.exp_desc = PT.Variable id; _ },
          { PT.exp_desc = PT.Variable arg; PT.exp_pos = arg_pos },
          _
        ) ->
          let id' = Ident.of_string id in
          let arg' = Ident.of_string arg in
          let body = exp_to_ast env rhs in
          let arg_tp = Type.Variable (Type.TypeVariable.create arg) in
          let ret_tp = AST.to_type (Env.add arg' arg_tp env) body in
          let fn_tp = Type.Function (arg_tp, ret_tp) in
          let fn = {
            AST.exp_desc = AST.Abstraction (arg', arg_tp, body) ;
            AST.exp_pos = arg_pos ;
          } in
          let env' = Env.add id' fn_tp env in
          let desc = AST.Binding (id', fn_tp, fn, AST.top_tag) in
          env', desc
        | _ ->
          failwith (
            Printf.sprintf
              "%s: expected a variable or function declaration"
              (Position.to_string lhs.PT.exp_pos)
          )
      in
      let hd' = { AST.exp_desc; AST.exp_pos = hd.PT.top_pos } in
      hd' :: top_to_ast env' tl
    | PT.Expression exp -> exp_to_ast env exp :: top_to_ast env tl

let f = top_to_ast Primative.tp_env
