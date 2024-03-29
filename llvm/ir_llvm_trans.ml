module IR = Internal_rep
module LL = Llvm
module StrMap = Ident.Map

let llctx = LL.global_context ()
let llmod = LL.create_module llctx "repl"
let llbld = LL.builder llctx

let int_type = LL.i32_type llctx
let bool_type = LL.i1_type llctx

let literal_to_llvm l = match l with
  | IR.Boolean false ->
    LL.const_int bool_type 0
  | IR.Boolean true ->
    LL.const_int bool_type 1
  | IR.Integer i ->
    LL.const_int int_type i

let binop_to_llvm d (op, e1, e2) =
  let llval_builder = match op with
    | Primative.Addition -> LL.build_add e1 e2
    | Primative.Subtraction -> LL.build_sub e1 e2
    | Primative.Multiplication -> LL.build_mul e1 e2
    | Primative.Division -> LL.build_sdiv e1 e2
    | Primative.Modulo -> LL.build_srem e1 e2
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
        failwith (Printf.sprintf
            "LlvmTrans.exp_to_llvalue: unknown variable \"%s\""
            (Ident.to_string id)
        )
    end
  | IR.Application (id, args) ->
    let fn = match LL.lookup_function (Ident.to_string id) llmod with
      | Some fn -> fn
      | None ->
        failwith "LlvmTrans.exp_to_llvalue: IR.Application unknown function"
    in
    let args' = Array.of_list (List.map (exp_to_llvalue env) args) in
    LL.build_call fn args' "call" llbld
  | IR.Binding (id, value, exp) ->
    let llval = exp_to_llvalue env value in
    let env' = StrMap.add id llval env in
    exp_to_llvalue env' exp

let rec typo_to_llvm tp = match tp with
  | Type.Unit -> failwith "Ir_llvm_trans.typo_to_llvm: unit not implemented"
  | Type.Boolean -> bool_type
  | Type.Integer -> int_type
  | Type.Variable _ ->
    failwith "Ir_llvm_trans.typo_to_llvm: variable not implemented"
  | Type.Function (arg_tp, ret_tp) ->
    let arg_tp' = typo_to_llvm arg_tp in
    let ret_tp' = typo_to_llvm ret_tp in
    LL.function_type ret_tp' (Array.make 1 arg_tp')
  | Type.Tuple _ ->
    failwith "Ir_llvm_trans.typo_to_llvm: tuple not implemented"

let top_to_llvalue llmod env top = match top with
  | IR.VariableDecl (id, exp) ->
    let exp' = exp_to_llvalue env exp in
    let var = LL.define_global (Ident.to_string id) exp' llmod in
    StrMap.add id var env
  | IR.FunctionDecl (id, args, body, tp) ->
    (* Declaration *)
    let tp' = typo_to_llvm tp in
    let fn = LL.define_function (Ident.to_string id) tp' llmod in
    LL.position_at_end (LL.entry_block fn) llbld;

    (* Set up arguments *)
    let params = LL.params fn in
    List.iteri
      (fun i id -> LL.set_value_name id params.(i))
      (List.map Ident.to_string args);
    let env' = Array.fold_left
      (
        fun env arg ->
          let id = Ident.of_string (LL.value_name arg) in
          StrMap.add id arg env
      )
      env
      params
    in

    (* Definition *)
    let llval = exp_to_llvalue env' body in
    let _ = LL.build_ret llval llbld in
    StrMap.add id fn env
  | IR.Expression exp ->
    let tp = LL.function_type int_type (Array.make 0 int_type) in
    let fn = LL.declare_function "repl_main" tp llmod in
    let bb0 = LL.append_block llctx "entry" fn in
    LL.position_at_end bb0 llbld;
    let llval = exp_to_llvalue env exp in
    let _ = LL.build_ret llval llbld in
    env

let f ir =
  let env0 = StrMap.empty in
  let _ = List.fold_left (top_to_llvalue llmod) env0 ir in
  llmod
