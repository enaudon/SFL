(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
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
    let tt = Infer.infer pt in
    Printf.printf ">> Abs Syntax Tree:\n%s"
      (String.concat "" (List.map AST.top_to_string tt));
    Printf.printf ">> Type:\n%s\n"
      (String.concat "\n" (List.map TT.typo_to_string (List.map TT.to_typo tt)));
    let ir = List.map IR.top_of_tt tt in
    Printf.printf ">> IR: %s"
      (String.concat "" (List.map IR.top_to_string ir));
    let ll = LL.translate ir in
    Printf.printf ">> LLVM:\n%s" (Llvm.string_of_llmodule ll);
    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()
    | _ -> Printf.printf "Unknown error\n"; repl ()

let _ = repl()
