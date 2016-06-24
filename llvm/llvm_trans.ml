module IR = Internal_rep
module LL = Llvm
module StrMap = Map.Make (String)

let llctx = LL.global_context ()
let int_type = LL.i32_type llctx
let bool_type = LL.i1_type llctx

let literal_to_llvm l = match l with
  | IR.Integer i ->
    LL.const_int int_type i
  | IR.Boolean false ->
    LL.const_int bool_type 0
  | IR.Boolean true ->
    LL.const_int bool_type 1

let binop_to_llvm d (op, e1, e2) =
  let llbld = LL.builder llctx in
  let llval_builder = match op with
    | IR.Addition -> LL.build_add e1 e2
    | IR.Subtraction -> LL.build_sub e1 e2
    | IR.Multiplication -> LL.build_mul e1 e2
    | IR.Division -> LL.build_sdiv e1 e2
    | IR.Modulo -> LL.build_srem e1 e2
  in llval_builder d llbld

let rec exp_to_llvalue env ast = match ast with
  | IR.Literal l ->
    literal_to_llvm l
  | IR.BinaryOperation (op, lhs, rhs) ->
    let lhs' = exp_to_llvalue env lhs in
    let rhs' = exp_to_llvalue env rhs in
    binop_to_llvm "binop" (op, lhs', rhs')
  | IR.Variable (id) ->
    begin
      try
        StrMap.find id env
      with Not_found ->
        failwith "LlvmTrans.exp_to_llvalue: unknown variable"
    end
  | IR.Application _ ->
    failwith "LlvmTrans.exp_to_llvalue: application?"
  | IR.Binding (binds, exp) ->
    let bind_fn env (id, e) =
      let llval = exp_to_llvalue env e in
      StrMap.add id llval env
    in
    let env' = List.fold_left bind_fn env binds in
    exp_to_llvalue env' exp

(*
module TT = Type_tree
let typo_to_llvm tp = match tp with
  | TT.Boolean -> bool_type
  | TT.Integer -> int_type
  | TT.Variable _ ->
    failwith "TT.Variable not implemented"
  | TT.Function _ ->
    failwith "TT.Function not implemented"
  | TT.Product _ ->
    failwith "TT.Product not implemented"
  | TT.Disjunction _ ->
    failwith "TT.Disjunction not implemented"
*)

let translate ir =
  let env0 = StrMap.empty in

  let llmod = LL.create_module llctx "repl" in
  let llbld = LL.builder llctx in

  let tp = LL.function_type int_type (Array.make 0 int_type) in
  let fn = LL.declare_function "repl_main" tp llmod in

  let bb0 = LL.append_block llctx "entry" fn in
  LL.position_at_end bb0 llbld;
  let llval = exp_to_llvalue env0 ir in
  let _ = LL.build_ret llval llbld in
  llmod
