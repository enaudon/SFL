(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
module AST2 = Abs_syntax_tree2
module TT = Type_tree
module IR = Internal_rep
module LL = Llvm_trans


let parse s =
  let top_list = Parser.top_list Lexer.token (Lexing.from_string s) in
  List.rev top_list

let rec repl () =
  print_string "? ";
  let input = read_line () in
  if input = "" then () else
  try
    let pt = parse input in
    Printf.printf ">> Parse Tree:\n%s"
      (String.concat "" (List.map PT.top_to_string pt));

    (* Old pathway *)
    let tt = Infer.infer pt in
    Printf.printf ">> Abs Syntax Tree I:\n%s\n"
      (String.concat "\n" (List.map AST.exp_to_string tt));
    Printf.printf ">> Type:\n%s\n"
      (String.concat "\n"
        (List.map TT.typo_to_string (List.map TT.to_typo tt)));

    (* New pathway *)
    let ast = List.map PT.top_to_ast pt in
    Printf.printf ">> Abs Syntax Tree II:\n%s\n"
      (String.concat "\n" (List.map AST2.exp_to_string ast));
    let ast' = Infer2.infer ast in
    Printf.printf "  ----\n%s\n"
      (String.concat "\n" (List.map AST2.exp_to_string ast'));

    let ir = List.map IR.top_of_tt tt in
    Printf.printf ">> Internal Rep:\n%s"
      (String.concat "" (List.map IR.top_to_string ir));
    let ll = LL.translate ir in
    Printf.printf ">> LLVM:\n%s" (Llvm.string_of_llmodule ll);
    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()
    | _ -> Printf.printf "Unknown error\n"; repl ()

let _ = repl()
