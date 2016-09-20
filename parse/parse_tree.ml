(*
 * Parse Tree
 *)

type id = string

type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

and exp =
  | Variable of id
  | Literal of lit
  | BinaryOperation of binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp

type top =
  | Declaration of exp * exp
  | Expression of exp

let binop_to_string op = match op with
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
  | Modulo -> "%"

let rec lit_to_string (l : lit) : string = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string pt =
  (* Returns the parenthesized string representation of a parse tree *)
  let paren pt = match pt with
    | Variable x -> x
    | Literal l -> lit_to_string l
    | _ -> Printf.sprintf "(%s)" (exp_to_string pt)
  in
  match pt with
    | Variable x -> x
    | Literal l -> lit_to_string l
    | BinaryOperation (op, pt1, pt2) ->
      Printf.sprintf "%s %s %s"
        (exp_to_string pt1)
        (binop_to_string op)
        (exp_to_string pt2)
    | Application (pt1, pt2) ->
      Printf.sprintf "%s %s" (paren pt1) (exp_to_string pt2)
    | Abstraction (arg, body) ->
      Printf.sprintf "%s -> %s" arg (exp_to_string body)
    | Binding (binds, pt) ->
      let fn (id, e) = Printf.sprintf "%s = %s" id (exp_to_string e) in
      Printf.sprintf "let %s in %s"
        (String.concat "; " (List.map fn binds))
        (exp_to_string pt)

let top_to_string top = match top with
  | Declaration (exp1, exp2) ->
    Printf.sprintf "%s = %s\n" (exp_to_string exp1) (exp_to_string exp2)
  | Expression exp ->
    Printf.sprintf "%s\n" (exp_to_string exp)

module AST = Abs_syntax_tree2
module Env = AST.Env

let rec lit_to_ast env lit = match lit with
  | Boolean b -> AST.Boolean b
  | Integer i -> AST.Integer i
  | Tuple es ->
    let es' = List.map (exp_to_ast env) es in
    AST.Tuple es'

and exp_to_ast env exp = match exp with
  | Variable id -> AST.Variable (id)
  | Literal l ->
    let l' = lit_to_ast env l in
    AST.Literal (l')
  | BinaryOperation (op, e1, e2) ->
    exp_to_ast env (
      Application (
        Application (Variable (binop_to_string op), e1),
        e2
      )
    )
  | Application (fn, arg) ->
    let fn' = exp_to_ast env fn in
    let arg' = exp_to_ast env arg in
    AST.Application (fn', arg')
  | Abstraction (arg, body) ->
    let body' = exp_to_ast env body in
    let arg_tp = Type.Variable (Type.TypeVariable.create arg) in
    let ret_tp = AST.to_type env body' in
    let fn_tp = Type.Function (arg_tp, ret_tp) in
    AST.Abstraction (arg, fn_tp, body')
  | Binding (binds, body) ->
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

let top_to_ast =
  let top_tag = AST.top_tag in
  let rec helper env exps = match exps with
    | [] -> []
    | hd :: tl ->
      match hd with
        | Declaration (Variable id, value) ->
          let value' = exp_to_ast env value in
          let tp = AST.to_type env value' in
          let env' = Env.add id tp env in
          AST.Binding (id, tp, value', top_tag) :: helper env' tl
        | Declaration (Application (Variable id, Variable arg), body) ->
          let body' = exp_to_ast env body in
          let arg_tp = Type.Variable (Type.TypeVariable.create arg) in
          let ret_tp = AST.to_type (Env.add arg arg_tp env) body' in
          let fn_tp = Type.Function (arg_tp, ret_tp) in
          let env' = Env.add id fn_tp env in
          let fn = AST.Abstraction (arg, arg_tp, body') in
          AST.Binding (id, fn_tp, fn, top_tag) :: helper env' tl
        | Declaration _ ->
          failwith "Expected a variable of function declaration"
        | Expression exp ->
          exp_to_ast env exp :: helper env tl
    in
    helper AST.empty_env
