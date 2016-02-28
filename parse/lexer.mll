{

open Parser
open Lexing
exception Eof

let create_integer n lexbuf = 
  try
    INTEGER (int_of_string n)
  with Failure _ ->
    let start = lexeme_start_p lexbuf in
    let finish = lexeme_end_p lexbuf in
    failwith ( Printf.sprintf
      "%s (%d,%d - %d,%d): invalid number '%s'"
      start.pos_fname
      start.pos_lnum start.pos_cnum
      finish.pos_lnum finish.pos_cnum
      (lexeme lexbuf)
    )

}


let identifier = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let decnum = ['0'-'9']*


rule token = parse
  | [' ' '\t']    { token lexbuf } (* skip blanks *)
  | ['\n']        { EOL }

  | '('           { LPAREN }
  | ')'           { RPAREN }
  | "->"          { IMP }
  | "="           { EQUAL }

  | "let"         { LET }
  | "in"          { IN }
  | "end"         { END }

  | "false"       { BOOLEAN false }
  | "true"        { BOOLEAN true }
  | decnum as n   { create_integer n lexbuf }
  | identifier as id  { VAR id }

  | eof           { EOL }

  | _ {
      let start = lexeme_start_p lexbuf in
      let finish = lexeme_end_p lexbuf in
      failwith ( Printf.sprintf
        "%s (%d,%d - %d,%d): unexpected character '%s'"
        start.pos_fname
        start.pos_lnum start.pos_cnum
        finish.pos_lnum finish.pos_cnum
        (lexeme lexbuf)
      )
    }
