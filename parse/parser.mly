%{

module PT = Parse_tree
module Pos = Position
open Lexing

let error msg nterm =
  let start = Parsing.rhs_start_pos nterm in
  let finish = Parsing.rhs_end_pos nterm in
  Printf.sprintf "%s (%d,%d - %d,%d): %s"
    start.pos_fname
    start.pos_lnum start.pos_cnum
    finish.pos_lnum finish.pos_cnum
    msg

let pos_of_nterm nterm =
  let start = Parsing.rhs_start_pos nterm in
  let finish = Parsing.rhs_end_pos nterm in
  Pos.create
      start.pos_fname
      (start.pos_lnum, start.pos_cnum)
      (finish.pos_lnum, finish.pos_cnum)

let create_top top_desc nterm =
  let top_pos = pos_of_nterm nterm in
  { PT.top_desc; PT.top_pos }

let create_exp exp_desc nterm =
  let exp_pos = pos_of_nterm nterm in
  { PT.exp_desc; PT.exp_pos }

let create_lit lit_desc nterm =
  let lit_pos = pos_of_nterm nterm in
  { PT.lit_desc; PT.lit_pos }

let create_binop (op_str, op_nterm) (lhs, lhs_nterm) (rhs, _) =
  let op = Primative.binop_to_string op_str in
  PT.Application (
    create_exp
      (PT.Application ( create_exp (PT.Variable op) op_nterm, lhs ))
      lhs_nterm,
    rhs
  )

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

%nonassoc LET ABS
%nonassoc LITERAL
%nonassoc VAR BOOLEAN INTEGER
%left PLUS MINUS
%left ASTERIK FSLASH PERCENT
%nonassoc LPAREN RPAREN
%left APP

%start top_list
%type <Parse_tree.top list> top_list

%%

top_desc:
  | exp EQUAL exp
    { PT.Declaration ($1, $3) }
  | exp
    { PT.Expression $1 }
  | error
    { failwith (error "expected declaration or expression" 1) }
;
top:
  | top_desc                { create_top $1 1 }
;
top_list:
  | top                     { [$1] }
  | top_list SEMICOLON top  { $3 :: $1 }
;

exp_desc:
  | lit %prec LITERAL { PT.Literal $1 }
  | VAR               { PT.Variable $1 }
  | exp PLUS exp
    { create_binop (Primative.Addition, 2) ($1, 1) ($3, 3) }
  | exp MINUS exp
    { create_binop (Primative.Subtraction, 2) ($1, 1) ($3, 3) }
  | exp ASTERIK exp
    { create_binop (Primative.Multiplication, 2) ($1, 1) ($3, 3) }
  | lit exp %prec ASTERIK
    {
      create_binop
        (Primative.Multiplication, 2)
        (create_exp (PT.Literal $1) 1, 1)
        ($2, 2)
     }
  | exp FSLASH exp
    { create_binop (Primative.Division, 2) ($1, 1) ($3, 3) }
  | exp PERCENT exp
    { create_binop (Primative.Modulo, 2) ($1, 1) ($3, 3) }
  | exp exp %prec APP       { PT.Application ($1, $2) }
  | VAR IMP exp %prec ABS   { PT.Abstraction ($1, $3) }
  | LET binding_list IN exp %prec LET   { PT.Binding (List.rev $2, $4) }
;
exp:
  | exp_desc                { create_exp $1 1 }
  | LPAREN exp_desc RPAREN  { create_exp $2 2 }
;

binding_list:
  | VAR EQUAL exp                         { [$1, $3] }
  | binding_list SEMICOLON VAR EQUAL exp  { ($3, $5) :: $1 }
;

lit_desc:
  | INTEGER   { PT.Integer $1 }
  | BOOLEAN   { PT.Boolean $1 }
;
lit:
  | lit_desc                { create_lit $1 1 }
;
