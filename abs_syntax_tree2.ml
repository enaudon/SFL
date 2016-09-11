(*
 *  Abstract Syntax Tree
 *)


type id = string

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

and exp =
  | Variable of id
  | Literal of lit
  | Application of exp * exp
  | Abstraction of id * Ast_type.t * exp
  | Binding of id * Ast_type.t * exp * exp

let top_tag = Variable ""

module type ENV = Map.S with type key = string
module Env = Map.Make (String)

let tp_of_variable env id =
  try Env.find id env
  with Not_found ->
    failwith (Printf.sprintf "Unbound variable \"%s\"" id)

let rec tp_of_exp env e = match e with
  | Variable id -> tp_of_variable env id
  | Literal l ->
    begin match l with
      | Boolean _ -> Ast_type.Boolean
      | Integer _ -> Ast_type.Integer
      | Tuple es -> Ast_type.Tuple (List.map (tp_of_exp env) es)
    end
  | Application (exp1, exp2) ->
    let tp1 = tp_of_exp env exp1 in
    let tp2 = tp_of_exp env exp2 in
    begin match tp1 with
      | Ast_type.Function (arg_tp, ret_tp) ->
        if arg_tp <> tp2
          then failwith "Unexpected argument type!"
          else ret_tp
      | _ -> failwith "Cannot apply non-function"
    end
  | Abstraction (arg, tp, body) ->
    let env' = Env.add arg tp env in
    let tp' = tp_of_exp env' body in
    Ast_type.Function (tp, tp')
  | Binding (id, tp, _, exp) ->
    let env' = Env.add id tp env in
    tp_of_exp env' exp

let type_check exp =
  tp_of_exp Env.empty exp

let rec constrain_exp env exp = match exp with
  | Variable id ->
    let tp = tp_of_variable env id in
    tp, []
  | Literal l ->
    begin match l with
      | Boolean _ -> Ast_type.Boolean, []
      | Integer _ -> Ast_type.Integer, []
      | Tuple es ->
        let es' = List.map (constrain_exp env) es in
        let ts = List.map fst es' in
        let cs = List.concat (List.map snd es') in
        (Ast_type.Tuple ts), cs
    end
  | Application (func, arg) ->
    let func_tp, func_cs = constrain_exp env func in
    let arg_tp, arg_cs = constrain_exp env arg in
    let ret_tp = Ast_type.Variable (Ast_type.TypeVariable.create "ret") in
    let cs =
      (func_tp, Ast_type.Function (arg_tp, ret_tp)) :: func_cs @ arg_cs
    in
    ret_tp, cs
  | Abstraction (arg, tp, body) ->
    let env' = Env.add arg tp env in
    let tp', cs = constrain_exp env' body in
    Ast_type.Function (tp, tp'), cs
  | Binding (id, tp, value, exp) ->
    let env' = Env.add id tp env in
    let _, v_cs = constrain_exp env' value in
    let e_env, e_cs = constrain_exp env' exp in
    e_env, v_cs @ e_cs

let rec constrain_top
  : Ast_type.t Env.t -> exp list -> (Ast_type.t * Ast_type.t) list
  = fun env exps -> match exps with
    | [] -> []
    | e :: es ->
      let env', cs = match e with
        | Binding (id, tp, value, exp) when exp = top_tag ->
          let env' = Env.add id tp env in
          let _, cs = constrain_exp env' value in
          (env', cs)
        | _ -> 
          let _, cs = constrain_exp env e in
          (env, cs)
      in
      cs @ constrain_top env' es

let empty_env =
  let tp = Ast_type.Function (
    Ast_type.Integer,
    Ast_type.Function (Ast_type.Integer, Ast_type.Integer)
  ) in
  List.fold_left
    (fun acc id -> Env.add id tp acc) 
    (Env.empty)
    ["+"; "-"; "*"; "/"; "%"]

let constrain exp = constrain_top empty_env exp

let rec lit_to_string l = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string e =
  let paren e = match e with
    | Variable x -> x
    | Literal l -> lit_to_string l
    | _ -> Printf.sprintf "(%s)" (exp_to_string e)
  in
  match e with
    | Variable id -> id
    | Literal l -> lit_to_string l
    | Application (exp1, exp2) ->
      Printf.sprintf "%s %s"
        (paren exp1)
        (paren exp2)
    | Abstraction (arg, tp, body) ->
      Printf.sprintf "%s : %s -> %s"
        arg
        (Ast_type.to_string tp)
        (exp_to_string body)
    | Binding (id, tp, value, exp) ->
      Printf.sprintf "let %s : %s = %s in %s"
        id
        (Ast_type.to_string tp)
        (exp_to_string value)
        (exp_to_string exp)
