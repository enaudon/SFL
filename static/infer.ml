(*
 *  Type Inference
 *  Parse Tree -> Type Tree
 *)

module PT = Parse_tree
module TT = Type_tree
module AST = Abs_syntax_tree

module type ENV = Map.S with type key = string
module Env = Map.Make (String)


let boolean_id = Unify.Identifier.fresh ()
let integer_id = Unify.Identifier.fresh ()
let tuple_id = Unify.Identifier.fresh ()
let function_id = Unify.Identifier.fresh ()

(* ---- *)
let rec term_to_string (t : Unify.term) : string = match t with
  | Unify.Function (id, tms) ->
    let tms_strs = Array.to_list (Array.map term_to_string tms) in
    begin match id with
      | _ when id = boolean_id -> "bool"
      | _ when id = integer_id -> "int"
      | _ when id = tuple_id ->
        Printf.sprintf "(%s)" (String.concat "," tms_strs)
      | _ when id = function_id ->
        Printf.sprintf "%s" (String.concat "->" tms_strs)
      | _ -> Unify.term_to_string t
    end
  | _ -> Unify.term_to_string t

let rec lit_to_string (l : 'a AST.lit) : string = match l with
  | AST.Boolean b -> string_of_bool b
  | AST.Integer i -> string_of_int i
  | AST.Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string tt = match tt with
  | AST.Variable (id, tp) ->
    Printf.sprintf "%s : %s" id (term_to_string tp)
  | AST.Literal (l, tp) ->
    Printf.sprintf "%s : %s" (lit_to_string l) (term_to_string tp)
  | AST.Application (e1, e2, tp) ->
    Printf.sprintf "((%s %s) : %s)"
      (exp_to_string e1)
      (exp_to_string e2)
      (Unify.term_to_string tp)
  | AST.Abstraction (arg, body, tp) ->
    Printf.sprintf "((%s -> %s) : %s)"
      arg
      (exp_to_string body)
      (Unify.term_to_string tp)
  | AST.Binding (id, value, e, _) ->
    Printf.sprintf "let %s = %s in %s"
      id
      (exp_to_string value)
      (exp_to_string e)

let constraint_to_string (tm1, tm2) =
    Printf.sprintf "%s = %s"
      (Unify.term_to_string tm1)
      (Unify.term_to_string tm2)
(* ---- *)

let typo_of_term (t : Unify.term) : TT.typo =
  let env =  Hashtbl.create 16 in
  let rec fn t = match t with
    | Unify.Variable (id) ->
      let tv =
        try
          Hashtbl.find env id
        with Not_found ->
          let tv = TT.TypeVariable.create "Type" in
          Hashtbl.replace env id tv;
          tv
      in
      TT.Variable (tv)
    | Unify.Function (id, terms) ->
      begin match id with
        | _ when id = boolean_id -> 
          TT.Boolean
        | _ when id = integer_id -> 
          TT.Integer
        | _ when id = tuple_id -> 
          let typos = Array.to_list (Array.map fn terms) in
          TT.Product typos
        | _ when id = function_id -> 
          let typos = Array.map fn terms in
          let tp1 = Array.get typos 0 in
          let tp2 = Array.get typos 1 in
          TT.Function (tp1, tp2)
        | _ -> failwith "Inference bug"
      end
    | Unify.Disjunction (terms) ->
      let typos = List.map fn terms in
      TT.Disjunction (typos)
  in
  fn t

let rec constrain_lit
  (c : (Unify.term * Unify.term) list)
  (l : Unify.term AST.lit)
  : (Unify.term * Unify.term) list
=
  match l with
  | AST.Boolean _ -> c
  | AST.Integer _ -> c
  | AST.Tuple es -> constrain_exp c es

and constrain_exp
  (c : (Unify.term * Unify.term) list)
  (e : Unify.term AST.exp list)
  : (Unify.term * Unify.term) list
=
  match e with
    | [] -> c
    | AST.Variable _ :: es ->
      constrain_exp c es
    | AST.Literal (l, _) :: es ->
      let c' = constrain_lit c l in
      constrain_exp c' es
    | AST.Application (exp1, exp2, tp) :: es ->
      let exp1_tp = AST.data exp1 in
      let exp2_tp = AST.data exp2 in
      let terms = Array.of_list [exp2_tp; tp] in
      let c' = (exp1_tp, Unify.funktion (function_id, terms)) :: c in
      let e' = exp2 :: exp1 :: es in
      constrain_exp c' e'
    | AST.Abstraction (_, body, _) :: es ->
      let e' = (body :: es) in
      constrain_exp c e'
    | AST.Binding (_, value, exp, _) :: es ->
      let es' = value :: exp :: es in
      constrain_exp c es'

let constrain e = constrain_exp [] [e]

let rec annotate_literal
  (env : Unify.term Env.t)
  (l : PT.lit)
  : Unify.term AST.exp
= match l with
  | PT.Boolean b ->
    let b' = AST.Boolean b in
    AST.Literal (b', Unify.constant boolean_id)
  | PT.Integer i ->
    let i' = AST.Integer i in
    AST.Literal (i', Unify.constant integer_id)
  | PT.Tuple es ->
    let es' = List.map (annotate_expression env) es in
    let tms = Array.of_list (List.map AST.data es') in
    AST.Literal (AST.Tuple es', Unify.funktion (tuple_id, tms))


and annotate_expression
  (env : Unify.term Env.t)
  (e : PT.exp)
  : Unify.term AST.exp
=
(*
  let env_add env id tm =
    let tm' = 
      try
        match Env.find id env with
          | Unify.Disjunction tms ->
            Unify.Disjunction (tm :: tms)
          | old_tm when old_tm <> tm ->
            Unify.Disjunction [tm; old_tm]
          | old_tm (* when old_tm = tm *) ->
            old_tm
      with Not_found ->
        tm
    in
    Env.add id tm' env
  in
*)
  match e with
    | PT.Variable (id) ->
      begin
        try
          let tm = Env.find id env in
          AST.Variable (id, tm)
        with Not_found ->
          failwith (Printf.sprintf "Unbound variable %s" id)
      end
    | PT.Literal (l) -> annotate_literal env l
    | PT.BinaryOperation (op, exp1, exp2) ->
      annotate_expression env (
        PT.Application (
          PT.Variable (PT.binop_to_string op),
          PT.Literal (PT.Tuple [exp1; exp2])
        )
      )
    | PT.Application (exp1, exp2) ->
      let tm = Unify.variable (Unify.Identifier.fresh ()) in
      let exp1' = annotate_expression env exp1 in
      let exp2' = annotate_expression env exp2 in
      AST.Application (exp1', exp2', tm)
    | PT.Abstraction (arg, body) ->
      let tm = Unify.variable (Unify.Identifier.fresh ()) in
      let env' = Env.add arg tm env in
      let body' = annotate_expression env' body in
      let terms = Array.of_list [tm; AST.data body'] in
      AST.Abstraction (arg, body', Unify.funktion (function_id, terms))
    | PT.Binding (binds, exp) ->
      let rec fn env binds = match binds with
        | [] -> annotate_expression env exp
        | (id, value) :: tl ->
          let value' = annotate_expression env value in
          let env' = Env.add id (AST.data value') env in
          let exp' = fn env' tl in
          AST.Binding (id, value', exp', AST.data exp')
      in
      fn env binds

let rec annotate_top
  (env : Unify.term Env.t)
  (tops : PT.top list)
  : Unify.term AST.exp list
=
  let dummy_exp =
    AST.Variable ("", Unify.variable (Unify.Identifier.fresh ()))
  in
  match tops with
    | [] -> []
    | hd :: tl -> match hd with
      | PT.Declaration (PT.Variable id, value) ->
        let value' = annotate_expression env value in
        let tm = AST.data value' in
        let env' = Env.add id tm env in
        AST.Binding (id, value', dummy_exp, tm) :: annotate_top env' tl
      | PT.Declaration (PT.Application (PT.Variable id, PT.Variable arg), body) ->
        let arg_tm = Unify.variable (Unify.Identifier.fresh ()) in
        let body' = annotate_expression (Env.add arg arg_tm env) body in
        let fun_tm = Unify.funktion (
          function_id,
          Array.of_list [arg_tm; AST.data body']
        ) in
        let env' = Env.add id fun_tm env in
        let fn = AST.Abstraction (arg, body', fun_tm) in
        AST.Binding (id, fn, dummy_exp, fun_tm) :: annotate_top env' tl
      | PT.Declaration _ ->
        failwith "Expected a variable of function declaration"
      | PT.Expression exp ->
        annotate_expression env exp :: annotate_top env tl

let empty_env =
  let tp = Array.of_list [
    Unify.Function (
      tuple_id,
      Array.of_list [
        Unify.constant integer_id;
        Unify.constant integer_id
      ]
    );
    Unify.constant integer_id
  ] in
  List.fold_left
    (fun acc id -> Env.add id (Unify.Function (function_id, tp)) acc) 
    (Env.empty)
    ["+"; "-"; "*"; "/"; "%"]

let infer e =
  let tt = annotate_top empty_env e in
  let cs = List.flatten (List.map constrain tt) in
  let s = Unify.unify (cs) in
  let tt' = List.map (AST.map (Unify.Substitution.apply s)) tt in
  let tt'' = List.map (AST.map typo_of_term) tt' in
  tt''
