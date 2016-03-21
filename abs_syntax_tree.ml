(*
 *  Abstract Syntax Tree
 *)


type id = string

type 'a literal =
  | Boolean of bool
  | Integer of int
  | Tuple of 'a t list

and 'a t =
  | Variable of id * 'a
  | Literal of 'a literal * 'a
  | Abstraction of id * 'a t * 'a
  | Application of 'a t * 'a t * 'a
  | Declaration of id * 'a t * 'a t * 'a

let data e = match e with
  | Variable (_, data) -> data
  | Literal (_, data) -> data
  | Abstraction (_, _, data) -> data
  | Application (_, _, data) -> data
  | Declaration (_, _, _, data) -> data

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
  | Abstraction (id, exp, data) ->
    let data' = fn data in
    let exp' = map fn exp in
    Abstraction (id, exp', data')
  | Application (exp1, exp2, data) ->
    let data' = fn data in
    let exp1' = map fn exp1 in
    let exp2' = map fn exp2 in
    Application (exp1', exp2', data')
  | Declaration (id, exp1, exp2, data) ->
    let data' = fn data in
    let exp1' = map fn exp1 in
    let exp2' = map fn exp2 in
    Declaration (id, exp1', exp2', data')
