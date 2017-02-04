(*
 *  Type Inference
 *  Parse Tree -> Type Tree
 *)

module AST = Abs_syntax_tree

let unit_id = Unify.Identifier.fresh ()
let boolean_id = Unify.Identifier.fresh ()
let integer_id = Unify.Identifier.fresh ()
let tuple_id = Unify.Identifier.fresh ()
let function_id = Unify.Identifier.fresh ()

let tv_env = Hashtbl.create 16
let tm_env = Hashtbl.create 16
let add tv tm =
  Hashtbl.replace tv_env (Unify.Identifier.to_string tm) tv;
  Hashtbl.replace tm_env (Type.TypeVariable.to_string tv) tm
let find_tv tm =
  let id = Unify.Identifier.to_string tm in
  try
    Hashtbl.find tv_env id
  with Not_found ->
    failwith (Printf.sprintf "type variable \"%s\" not found" id)
let find_tm tv =
  let id = Type.TypeVariable.to_string tv in
  try
    Hashtbl.find tm_env id
  with Not_found ->
    let tm = Unify.Identifier.fresh () in
    add tv tm;
    tm

let rec term_of_typo tp = match tp with
  | Type.Unit -> Unify.constant unit_id
  | Type.Boolean -> Unify.constant boolean_id
  | Type.Integer -> Unify.constant integer_id
  | Type.Variable id ->
    Unify.variable (find_tm id)
  | Type.Function (arg, ret) ->
    let arg_tm = term_of_typo arg in
    let ret_tm = term_of_typo ret in
    Unify.funktion (function_id, Array.of_list [arg_tm; ret_tm])
  | Type.Tuple tps ->
    let tms = List.map term_of_typo tps in
    Unify.funktion (tuple_id, Array.of_list tms)

let rec typo_of_term t = match t with
  | Unify.Variable (id) -> Type.Variable (find_tv id)
  | Unify.Function (id, terms) ->
    begin match id with
      | _ when id = unit_id -> 
        Type.Unit
      | _ when id = boolean_id -> 
        Type.Boolean
      | _ when id = integer_id -> 
        Type.Integer
      | _ when id = tuple_id -> 
        let typos = Array.to_list (Array.map typo_of_term terms) in
        Type.Tuple typos
      | _ when id = function_id -> 
        let typos = Array.map typo_of_term terms in
        let tp1 = Array.get typos 0 in
        let tp2 = Array.get typos 1 in
        Type.Function (tp1, tp2)
      | _ -> failwith "Inference bug"
    end
  | Unify.Disjunction _ ->
    failwith "Disjunction"

let rec ast_map fn ast =
  let exp_desc = match ast.AST.exp_desc with
    | AST.Abstraction (arg, tp, body) ->
      let tp' = fn tp in
      let body' = ast_map fn body in
      AST.Abstraction (arg, tp', body')
    | AST.Binding (id, tp, value, exp) ->
      let tp' = fn tp in
      let value' = ast_map fn value in
      let exp' = ast_map fn exp in
      AST.Binding (id, tp', value', exp')
    | desc -> desc
  in
  { ast with AST.exp_desc }

let map_fn s tp =
  let tm = term_of_typo tp in
  let tm' = Unify.Substitution.apply s tm in
  typo_of_term tm'

let infer ast =
  let cs = AST.constrain ast in
  List.iter
    (
      fun (t1, t2) -> Printf.printf "%s = %s\n"
        (Type.to_string t1)
        (Type.to_string t2)
    )
    cs;
  let s = Unify.unify (List.map
    (fun (tp1, tp2) -> term_of_typo tp1, term_of_typo tp2)
    cs
  ) in
  List.map (ast_map (map_fn s)) ast
