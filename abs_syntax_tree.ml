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

let data e = match e with
  | Variable (_, data) -> data
  | Literal (_, data) -> data
  | Application (_, _, data) -> data
  | Binding (_, _, data) -> data

let rec map fn e = match e with
  | Variable (id, data) ->
    let data' = fn data in
    Variable (id, data')
  | Literal (l, data) ->
    let helper l = match l with
      | Boolean b -> Boolean b
      | Integer i -> Integer i
      | Tuple es -> Tuple (List.map (map fn) es)
    in
    let l' = helper l in
    let data' = fn data in
    Literal (l', data')
  | Application (exp1, exp2, data) ->
    let data' = fn data in
    let exp1' = map fn exp1 in
    let exp2' = map fn exp2 in
    Application (exp1', exp2', data')
  | Binding (binds, exp, data) ->
    let data' = fn data in
    let binds' = List.map (fun (id, e) -> id, map fn e) binds in
    let exp' = map fn exp in
    Binding (binds', exp', data')
