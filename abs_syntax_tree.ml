(*
 *  Abstract Syntax Tree
 *)


type id = string

type 'a literal =
  | Boolean of bool
  | Integer of int
  | Tuple of 'a exp list

and 'a exp =
  | Variable of id * 'a
  | Literal of 'a literal * 'a
  | Application of 'a exp * 'a exp * 'a
  | Binding of (id * 'a exp) list * 'a exp * 'a

type 'a top =
  | VariableDecl of id * 'a exp * 'a
  | FunctionDecl of id * id list * 'a exp * 'a
  | Expression of 'a exp * 'a

let exp_data e = match e with
  | Variable (_, data) -> data
  | Literal (_, data) -> data
  | Application (_, _, data) -> data
  | Binding (_, _, data) -> data

let top_data e = match e with
  | VariableDecl (_, _, data) -> data
  | FunctionDecl (_, _, _, data) -> data
  | Expression (_, data) -> data

let rec map fn (t : 'a top) =
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
