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
  | [' ' '\t']    { token lexbuf }
  | ['\n']        { Printf.printf "\n%!"; token lexbuf }

  | "+"           { Printf.printf "PLUS %!"; PLUS }
  | "-"           { Printf.printf "MINIUS %!"; MINUS }
  | "*"           { Printf.printf "ASTERIK %!"; ASTERIK }
  | "/"           { Printf.printf "FSLASH %!"; FSLASH }
  | "%"           { Printf.printf "PER %!"; PERCENT }

  | "&"           { Printf.printf "AMP %!"; AMPERSAND }
  | "|"           { Printf.printf "OBEL %!"; OBELISK }

  | ','           { Printf.printf "COMMA %!"; COMMA }
  | ';'           { Printf.printf "SEMI %!"; SEMICOLON }

  | '('           { Printf.printf "LPAREN %!"; LPAREN }
  | ')'           { Printf.printf "RPAREN %!"; RPAREN }
  | '{'           { Printf.printf "LBRACE %!"; LBRACE }
  | '}'           { Printf.printf "RBRACE %!"; RBRACE }
  | '<'           { Printf.printf "LCHEVR %!"; LCHEVR }
  | '>'           { Printf.printf "RCHEVR %!"; RCHEVR }

  | "->"          { Printf.printf "IMP %!"; IMP }
  | "="           { Printf.printf "EQUAL %!"; EQUAL }

  | "let"         { Printf.printf "LET %!"; LET }
  | "in"          { Printf.printf "IN %!"; IN }
  | "end"         { Printf.printf "END %!"; END }

  | "false"       { Printf.printf "true %!"; BOOLEAN false }
  | "true"        { Printf.printf "false %!"; BOOLEAN true }
  | decnum as n   { Printf.printf "NUM %!"; create_integer n lexbuf }
  | identifier as id  { Printf.printf "VAR %!"; VAR id }

  | eof           { Printf.printf "\n%!"; EOF }

  | _ {
      Printf.printf "unexpected character %!";
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
