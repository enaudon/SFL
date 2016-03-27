(*
 *  Type Inference
 *  Parse Tree -> Type Tree
 *)

module PT = Parse_tree
module TT = Type_tree
module AST = Abs_syntax_tree
module StrMap = Map.Make (String)


let boolean_id = Unify.Identifier.fresh ()
let integer_id = Unify.Identifier.fresh ()
let tuple_id = Unify.Identifier.fresh ()
let function_id = Unify.Identifier.fresh ()

(* ---- *)
let rec term_to_string (t : Unify.term) : string = match t with
  | Unify.Function (id, tms) ->
    let tms_strs = Array.to_list (Array.map term_to_string tms) in
    begin match id with
      | id when id = boolean_id -> "bool"
      | id when id = integer_id -> "int"
      | id when id = tuple_id ->
        Printf.sprintf "(%s)" (String.concat "," tms_strs)
      | id when id = function_id ->
        Printf.sprintf "%s" (String.concat "->" tms_strs)
      | id -> Unify.term_to_string t
    end
  | _ -> Unify.term_to_string t

let rec literal_to_string (l : 'a AST.literal) : string = match l with
  | AST.Boolean b -> string_of_bool b
  | AST.Integer i -> string_of_int i
  | AST.Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string tt = match tt with
  | AST.Variable (id, tp) ->
    Printf.sprintf "%s : %s" id (term_to_string tp)
  | AST.Literal (l, tp) ->
    Printf.sprintf "%s : %s" (literal_to_string l) (term_to_string tp)
  | AST.Application (e1, e2, tp) ->
    Printf.sprintf "((%s %s) : %s)"
      (exp_to_string e1)
      (exp_to_string e2)
      (Unify.term_to_string tp)
  | AST.Binding (binds, e, _) ->
    let fn (id, e) = Printf.sprintf "%s = %s" id (exp_to_string e) in
    Printf.sprintf "let %s in %s"
      (String.concat "; " (List.map fn binds))
      (exp_to_string e)

and top_to_string tt = match tt with
  | AST.VariableDecl (id, exp, tm) ->
    Printf.sprintf "(%s = %s) : %s" id
      (exp_to_string exp)
      (Unify.term_to_string tm)
  | AST.FunctionDecl (id, args, body, tm) ->
    Printf.sprintf "(%s(args) = %s) : %s" id
      (exp_to_string body)
      (Unify.term_to_string tm)
  | AST.Expression (exp, tm) ->
    Printf.sprintf "%s : %s"
      (exp_to_string exp)
      (Unify.term_to_string tm)

let constraint_to_string (tm1, tm2) =
    Printf.sprintf "%s = %s"
      (Unify.term_to_string tm1)
      (Unify.term_to_string tm2)
(* ---- *)

let typo_of_term (t : Unify.term) : TT.typo =
  let env =  Hashtbl.create 16 in
  let rec fn t = match t with
    | Unify.Variable (id) ->
      let tv = begin
        try
          Hashtbl.find env id
        with Not_found ->
          let tv = TT.TypeVariable.create "Type" in
          Hashtbl.replace env id tv;
          tv
      end in
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
  (l : Unify.term AST.literal)
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
    let exp1_tp = AST.exp_data exp1 in
    let exp2_tp = AST.exp_data exp2 in
    let terms = Array.of_list [exp2_tp; tp] in
    let c' = (exp1_tp, Unify.funktion (function_id, terms)) :: c in
    let e' = exp2 :: exp1 :: es in
    constrain_exp c' e'
(*
  | AST.Abstraction (_, exp, _) :: es ->
    let e' = (exp :: es) in
    constrain_exp c e'
*)
  | AST.Binding (binds, exp, _) :: es ->
    let bind_fn (_, e) es = e :: es in
    let es' = List.fold_right bind_fn binds es in
    let e' = exp :: es' in
    constrain_exp c e'

let constrain_top
  (t : Unify.term AST.top)
  : (Unify.term * Unify.term) list
=
  let rec fn c t = match t with
    | [] -> c
    | AST.VariableDecl (_) :: es ->
      fn c es
    | AST.FunctionDecl (_, _, body, _) :: es ->
      let c' = constrain_exp c [body] in
      fn c' es
    | AST.Expression (exp, _) :: es ->
      let c' = constrain_exp c [exp] in
      fn c' es
  in
  let c0 = [] in
  let t0 = [t] in
  fn c0 t0

let rec annotate_literal
  (env : Unify.term StrMap.t)
  (l : PT.literal)
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
    let tms = Array.of_list (List.map AST.exp_data es') in
    AST.Literal (AST.Tuple es', Unify.funktion (tuple_id, tms))


and annotate_expression
  (env : Unify.term StrMap.t)
  (e : PT.exp)
  : Unify.term AST.exp
=
(*
  let env_add env id tm =
    let tm' = 
      try
        match StrMap.find id env with
          | Unify.Disjunction tms ->
            Unify.Disjunction (tm :: tms)
          | old_tm when old_tm <> tm ->
            Unify.Disjunction [tm; old_tm]
          | old_tm (* when old_tm = tm *) ->
            old_tm
      with Not_found ->
        tm
    in
    StrMap.add id tm' env
  in
*)
  match e with
    | PT.Variable (id) ->
      begin
        try
          let tm = StrMap.find id env in
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
(*
    | PT.Abstraction (id, exp) ->
      let tm = Unify.variable (Unify.Identifier.fresh ()) in
      let env' = env_add env id tm in
      let exp' = fn env' exp in
      let terms = Array.of_list [tm; AST.exp_data exp'] in
      AST.Abstraction (id, exp', Unify.funktion (function_id, terms))
*)
    | PT.Binding (binds, exp) ->
      let bind_fn (env, binds) (id, exp) = 
        let exp' = annotate_expression env exp in
        StrMap.add id (AST.exp_data exp') env, (id, exp') :: binds
      in
      let env', binds' = List.fold_left bind_fn (env, []) binds in
      let exp' = annotate_expression env' exp in
      AST.Binding (binds', exp', AST.exp_data exp')

let annotate_top
  (env : Unify.term StrMap.t)
  (t : PT.top)
  : Unify.term AST.top
= match t with
  | PT.Declaration (PT.Variable id, exp) ->
    let exp' = annotate_expression env exp in
    let tp = AST.exp_data exp' in
    let env' = StrMap.add id tp env in
    AST.VariableDecl (id, exp', tp)
  | PT.Declaration (PT.Application (PT.Variable id, PT.Variable arg), exp) ->
    let tm = Unify.variable (Unify.Identifier.fresh ()) in
    let env' = StrMap.add arg tm env in
    let exp' = annotate_expression env' exp in
    let terms = Array.of_list [tm; AST.exp_data exp'] in
    AST.FunctionDecl (id, [arg], exp', Unify.funktion (function_id, terms))
  | PT.Declaration _ ->
    failwith "Expected a variable of function declaration"
  | PT.Expression exp ->
    let exp' = annotate_expression env exp in
    AST.Expression (exp', AST.exp_data exp')

let env0 =
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
    (fun acc id -> StrMap.add id (Unify.Function (function_id, tp)) acc) 
    (StrMap.empty)
    ["+"; "-"; "*"; "/"; "%"]

let infer e =
  let tt = annotate_top env0 e in
  let cs = constrain_top tt in
  let s = Unify.unify (cs) in
  let tt' = AST.map (Unify.Substitution.apply s) tt in
  let tt'' = AST.map typo_of_term tt' in
  tt''
