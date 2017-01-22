module AST = Abs_syntax_tree
module V = Value
module Env = Ident.Map

let val_env =
  let eval_prim op =
      Value.Function ( fun lhs -> Value.Function ( fun rhs ->
        match lhs, rhs with
          | Value.Integer i1, Value.Integer i2 ->
            Value.Integer (op i1 i2)
          | _ -> failwith "Primative.eval_prim: incorrect types"
      ) )
  in
  List.fold_left
    (fun acc (id, v) -> Ident.Map.add (Ident.of_string id) v acc)
    (Ident.Map.empty)
    [
      "+", eval_prim ( + );
      "-", eval_prim ( - );
      "*", eval_prim ( * );
      "/", eval_prim ( / );
      "%", eval_prim ( mod );
    ]

let eval_lit lit = match lit with
  | AST.Integer i -> V.Integer i
  | AST.Boolean b -> V.Boolean b
  | AST.Tuple _ ->
    failwith "Value.eval_lit: AST.Tuple to Value not implemented"

let eval_var env id =
  try Env.find id env
  with Not_found ->
    failwith (
      Printf.sprintf
        "Value.eval_var: unbound variable \"%s\""
        (Ident.to_string id)
    )

let rec eval_app env id body arg =
  let env' = Env.add id arg env in
  eval_exp env' body

and eval_exp env exp = match exp with
  | AST.Literal l -> eval_lit l
  | AST.Variable id -> eval_var env id
  | AST.Application (fn, arg) ->
    let arg' = eval_exp env arg in
    begin match eval_exp env fn with
      | V.Function fn' -> fn' arg'
      | _ -> failwith "Value.eval_exp: Cannot apply non-function value"
    end
  | AST.Abstraction (id, _, body) ->
    V.Function (eval_app env id body)
  | AST.Binding (id, _, value, exp) ->
    let value' = eval_exp env value in
    let env' = Env.add id value' env in
    eval_exp env' exp

let f = eval_exp val_env
