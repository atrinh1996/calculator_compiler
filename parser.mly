/* 
    parser

    CS107 - Arith Parser

    Name: Amy Bui (abui02)
*/



%{ open Ast %}
%token PLUS MINUS TIMES DIVIDE ASSIGN EOF
%token SEMI
%token COMMA
%token <string> VARIABLE
%token <string> FLOAT
%token <int> INT

%left   COMMA
%right  ASSIGN
%left   PLUS MINUS
%left   TIMES DIVIDE

%start prog
%type <Ast.prog> prog

%%

prog: 
    stmt_list EOF       { $1 }

stmt_list: 
                                { [] }
    | math_stmt stmt_list       { $1 :: $2 }

math_stmt:
    expr SEMI               { $1 }

expr: 
      expr     PLUS   expr  { Binop($1, Add, $3) }
    | expr     MINUS  expr  { Binop($1, Sub, $3) }
    | expr     TIMES  expr  { Binop($1, Mul, $3) }
    | expr     DIVIDE expr  { Binop($1, Div, $3) }
    | MINUS    expr         { Unop(Neg, $2) }
    | INT                   { Iliteral($1) }
    | FLOAT                 { Fliteral($1) }
    /* | expr     COMMA  expr  { Seq($1, $3) } */
    | VARIABLE ASSIGN expr  { Asn($1, $3) }
    | VARIABLE              { Var($1) }

