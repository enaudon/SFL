(*
 *  Abstract Syntax Tree
 *)


type id = string

type 'a lit =
  | Boolean of bool
  | Integer of int
  | Tuple of 'a exp list

and 'a exp =
  | Variable of id * 'a
  | Literal of 'a lit * 'a
  | Application of 'a exp * 'a exp * 'a
  | Abstraction of id * 'a exp * 'a
  | Binding of (id * 'a exp) list * 'a exp * 'a

type 'a top =
  | VariableDecl of id * 'a exp * 'a
  | FunctionDecl of id * id list * 'a exp * 'a
  | Expression of 'a exp * 'a

let exp_data e = match e with
  | Variable (_, data) -> data
  | Literal (_, data) -> data
  | Application (_, _, data) -> data
  | Abstraction (_, _, data) -> data
  | Binding (_, _, data) -> data

let top_data e = match e with
  | VariableDecl (_, _, data) -> data
  | FunctionDecl (_, _, _, data) -> data
  | Expression (_, data) -> data

let map fn (t : 'a top) =
  let rec exp_map fn e = match e with
    | Variable (id, data) ->
      let data' = fn data in
      Variable (id, data')
    | Literal (l, data) ->
      let helper l = match l with
        | Boolean b -> Boolean b
        | Integer i -> Integer i
        | Tuple es -> Tuple (List.map (exp_map fn) es)
      in
      let l' = helper l in
      let data' = fn data in
      Literal (l', data')
    | Application (exp1, exp2, data) ->
      let data' = fn data in
      let exp1' = exp_map fn exp1 in
      let exp2' = exp_map fn exp2 in
      Application (exp1', exp2', data')
    | Abstraction (arg, body, data) ->
      let data' = fn data in
      let body' = exp_map fn body in
      Abstraction (arg, body', data')
    | Binding (binds, exp, data) ->
      let data' = fn data in
      let binds' = List.map (fun (id, e) -> id, exp_map fn e) binds in
      let exp' = exp_map fn exp in
      Binding (binds', exp', data')
  in
  match t with
    | VariableDecl (id, exp, data) ->
      let data' = fn data in
      let exp' = exp_map fn exp in
      VariableDecl (id, exp', data')
    | FunctionDecl (id, args, body, data) ->
      let data' = fn data in
      let body' = exp_map fn body in
      FunctionDecl (id, args, body', data')
    | Expression (exp, data) ->
      let data' = fn data in
      let exp' = exp_map fn exp in
      Expression (exp', data')

let rec lit_to_string l = match l with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Tuple es ->
    Printf.sprintf "(%s)"
      (String.concat ", " (List.map exp_to_string es))

and exp_to_string e = match e with
  | Variable (id, _) -> id
  | Literal (l, _) -> lit_to_string l
  | Application (exp1, exp2, _) ->
    Printf.sprintf "%s(%s)"
      (exp_to_string exp1)
      (exp_to_string exp2)
  | Abstraction (arg, body, _) ->
    Printf.sprintf "(%s -> %s)" arg (exp_to_string body)
  | Binding (binds, exp, _) ->
    let fn (id, e) = Printf.sprintf "%s = %s" id (exp_to_string e) in
    Printf.sprintf "let %s in %s"
      (String.concat "; " (List.map fn binds))
      (exp_to_string exp)

let top_to_string t = match t with
  | VariableDecl (id, exp, _) ->
    Printf.sprintf "%s = %s\n" id (exp_to_string exp)
  | FunctionDecl (id, args, body, _) ->
    Printf.sprintf "%s(%s) = %s\n"
      id (String.concat ", " args) (exp_to_string body)
  | Expression (exp, _) ->
    Printf.sprintf "%s\n" (exp_to_string exp)
