(*
 *  Abstract Syntax Tree
 *)


type id = Ident.t

type lit_desc =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list
and lit = {
  lit_desc : lit_desc ;
  lit_pos : Position.t ;
}

and exp_desc =
  | Literal of lit
  | Variable of id
  | Application of exp * exp
  | Abstraction of id * Type.t * exp
  | Binding of id * Type.t * exp * exp
and exp = {
  exp_desc : exp_desc ;
  exp_pos : Position.t ;
}

let top_tag = {
  exp_desc = Variable (Ident.of_string "") ;
  exp_pos = Position.dummy ;
}

module Env = Ident.Map

let tp_of_variable env id =
  try Env.find id env
  with Not_found ->
    failwith (
      Printf.sprintf "Unbound variable \"%s\"" (Ident.to_string id)
    )

let tp_of_exp tpchk env e =
  let rec helper env e = match e.exp_desc with
    | Literal l ->
      begin match l.lit_desc with
        | Boolean _ -> Type.Boolean
        | Integer _ -> Type.Integer
        | Tuple es -> Type.Tuple (List.map (helper env) es)
      end
    | Variable id -> tp_of_variable env id
    | Application (fn, arg) ->
      begin match helper env fn with
        | Type.Function (arg_tp, ret_tp) ->
          if tpchk && arg_tp <> (helper env arg)
            then failwith "Argument and function types do not match!"
            else ret_tp
        | _ -> failwith "Cannot apply non-function"
      end
    | Abstraction (arg, tp, body) ->
      let env' = Env.add arg tp env in
      let tp' = helper env' body in
      Type.Function (tp, tp')
    | Binding (id, tp, _, exp) ->
      let env' = Env.add id tp env in
      helper env' exp
  in
  helper env e

let to_type = tp_of_exp false
let typecheck = tp_of_exp true

let to_type_list env0 es =
  let helper (env, tps) exp = match exp.exp_desc with
    | Binding (id, tp, _, exp) when exp = top_tag ->
      let env' = Ident.Map.add id tp env in
      (env', tp :: tps)
    | _ ->
      let tp = to_type env exp in
      (env, tp :: tps)
  in
  let _, ts = List.fold_left helper (env0, []) es in
  ts

let to_ident_list es =
  let helper exp = match exp.exp_desc with
    | Binding (id, _, _, exp) when exp = top_tag -> id
    | _ -> Ident.of_string "_"
  in
  List.map helper es

let rec constrain_exp env exp = match exp.exp_desc with
  | Variable id ->
    let tp = tp_of_variable env id in
    tp, []
  | Literal l ->
    begin match l.lit_desc with
      | Boolean _ -> Type.Boolean, []
      | Integer _ -> Type.Integer, []
      | Tuple es ->
        let es' = List.map (constrain_exp env) es in
        let ts = List.map fst es' in
        let cs = List.concat (List.map snd es') in
        (Type.Tuple ts), cs
    end
  | Application (func, arg) ->
    let func_tp, func_cs = constrain_exp env func in
    let arg_tp, arg_cs = constrain_exp env arg in
    let ret_tp = match func.exp_desc with
      | Variable id ->
        Type.Variable
          (Type.TypeVariable.create
            (Printf.sprintf "%s.ret" (Ident.to_string id)))
      | _ ->
        Type.Variable (Type.TypeVariable.create "ret")
    in
    let cs =
      (func_tp, Type.Function (arg_tp, ret_tp)) :: func_cs @ arg_cs
    in
    ret_tp, cs
  | Abstraction (arg, tp, body) ->
    let env' = Env.add arg tp env in
    let tp', cs = constrain_exp env' body in
    Type.Function (tp, tp'), cs
  | Binding (id, tp, value, exp) ->
    let env' = Env.add id tp env in
    let _, v_cs = constrain_exp env' value in
    let e_env, e_cs = constrain_exp env' exp in
    e_env, v_cs @ e_cs

let rec constrain_top
  : Type.t Env.t -> exp list -> (Type.t * Type.t) list
  = fun env exps -> match exps with
    | [] -> []
    | e :: es ->
      let env', cs = match e.exp_desc with
        | Binding (id, tp, value, exp) when exp = top_tag ->
          let env' = Env.add id tp env in
          let _, cs = constrain_exp env' value in
          (env', cs)
        | _ ->
          let _, cs = constrain_exp env e in
          (env, cs)
      in
      cs @ constrain_top env' es

let constrain exp = constrain_top Primative.tp_env exp

let rec format_lit ff l = match l.lit_desc with
  | Boolean b -> Format.fprintf ff "%b" b
  | Integer i -> Format.fprintf ff "%i" i
  | Tuple es ->
    let pp_sep ff () = Format.fprintf ff ",@ " in
    Format.fprintf ff "(%a)"
      (Format_util.format_list pp_sep format_exp) es

and format_exp ff exp =
  let paren ff exp = match exp.exp_desc with
    | Variable _
    | Literal _ -> format_exp ff exp
    | _ -> Format.fprintf ff "(%a)" format_exp exp
  in
  match exp.exp_desc with
    | Variable id -> Format.fprintf ff "%s" (Ident.to_string id)
    | Literal l -> format_lit ff l
    | Application _ ->
      let rec format_app ff exp = match exp.exp_desc with
        | Application (fn, arg) ->
          Format.fprintf ff "%a@ %a" format_app fn paren arg
        | _ -> paren ff exp
      in
      Format.fprintf ff "@[<hv 2>%a@]" format_app exp
    (* TODO: fix odd box opening/closing here--deets in comments *)
    | Abstraction _ ->
      let rec format_abs ff exp = match exp.exp_desc with
        | Abstraction (arg, tp, body) ->
          let formatter = match body.exp_desc with
            | Abstraction _ -> Format.fprintf ff "%s : %s ->@ %a"
            | _ -> Format.fprintf ff "%s : %s ->%a"
          in
          formatter
            (Ident.to_string arg)
            (Type.to_string tp)
            format_abs body
        (* The unmatched close closes the box below *)
        | _ -> Format.fprintf ff "@]@ %a" format_exp exp
      in
      (* The second box here (hov 2) is closed in the %a.  See above. *)
      Format.fprintf ff "@[<hv 2>@[<hov 2>%a@]" format_abs exp
    | Binding _ ->
      let rec format_bind ff exp = match exp.exp_desc with
        | Binding (id, tp, value, exp) ->
          Format.fprintf ff
            "@[<hv 0>@[<hv 2>let %s : %s =@ %a@]@ in@]@ %a"
            (Ident.to_string id)
            (Type.to_string tp)
            format_exp value
            format_bind exp
        | _ -> format_exp ff exp
      in
      Format.fprintf ff "@[<hv 0>%a@]" format_bind exp

let lit_to_string = Format.asprintf "%a" format_lit
let exp_to_string = Format.asprintf "%a" format_exp
