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
  | Binding of id * 'a exp * 'a exp * 'a

let top_tag data = Variable ("", data)

let data e = match e with
  | Variable (_, data) -> data
  | Literal (_, data) -> data
  | Application (_, _, data) -> data
  | Abstraction (_, _, data) -> data
  | Binding (_, _, _, data) -> data

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
  | Abstraction (arg, body, data) ->
    let data' = fn data in
    let body' = map fn body in
    Abstraction (arg, body', data')
  | Binding (id, value, exp, data) ->
    let data' = fn data in
    let value' = map fn value in
    let exp' = map fn exp in
    Binding (id, value', exp', data')

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
  | Binding (id, value, exp, _) ->
    Printf.sprintf "let %s = %s in %s"
      id
      (exp_to_string value)
      (exp_to_string exp)
