%{
module PT = Parse_tree
open Lexing

let error msg nterm =
  let start = Parsing.rhs_start_pos nterm in
  let finish = Parsing.rhs_end_pos nterm in
  Printf.sprintf "%s (%d,%d - %d,%d): %s"
    start.pos_fname
    start.pos_lnum start.pos_cnum
    finish.pos_lnum finish.pos_cnum
    msg

%}

%token <string> VAR
%token PLUS MINUS ASTERIK FSLASH PERCENT
%token AMPERSAND OBELISK
%token COMMA SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE LCHEVR RCHEVR
%token IMP
%token EQUAL
%token LET IN
%token <bool> BOOLEAN
%token <int> INTEGER
%token EOL EOF

%nonassoc LET
%nonassoc APP ABS
%nonassoc LITERAL
%nonassoc VAR BOOLEAN INTEGER
%nonassoc LPAREN RPAREN
%left PLUS MINUS
%left ASTERIK FSLASH PERCENT

%start top_list
%type <Parse_tree.top list> top_list

%%

top:
  | exp EQUAL exp
    { PT.Declaration ($1, $3) }
  | exp
    { PT.Expression $1 }
  | error
    { failwith (error "expected declaration or expression" 1) }
;
top_list:
  | top                     { [$1] }
  | top_list SEMICOLON top  { $3 :: $1 }
;

literal:
  | INTEGER   { PT.Integer $1 }
  | BOOLEAN   { PT.Boolean $1 }
;

exp:
  | literal %prec LITERAL { PT.Literal ($1) }
  | VAR               { PT.Variable $1 }
  | exp PLUS exp      { PT.BinaryOperation (PT.Addition, $1, $3) }
  | exp MINUS exp     { PT.BinaryOperation (PT.Subtraction, $1, $3) }
  | exp ASTERIK exp   { PT.BinaryOperation (PT.Multiplication, $1, $3) }
  | literal exp %prec ASTERIK
    { PT.BinaryOperation (PT.Multiplication, PT.Literal $1, $2) }
  | exp FSLASH exp    { PT.BinaryOperation (PT.Division, $1, $3) }
  | exp PERCENT exp   { PT.BinaryOperation (PT.Modulo, $1, $3) }
  | exp exp %prec APP       { PT.Application ($1, $2) }
  | VAR IMP exp %prec ABS   { PT.Abstraction ($1, $3) }
  | LET binding_list IN exp %prec LET   { PT.Binding (List.rev $2, $4) }
  | LPAREN exp RPAREN                   { $2 }
;

binding_list:
  | VAR EQUAL exp                         { [$1, $3] }
  | binding_list SEMICOLON VAR EQUAL exp  { ($3, $5) :: $1 }
;
