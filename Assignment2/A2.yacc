
%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int | IMPLIES | IF | THEN | ELSE | RPAREN | LPAREN | EOF 

%nonterm program | statement | formula | Expression | Conditional | Term | Factor | binOp

%pos int

%eop EOF
%noshift EOF

%start program

%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%verbose

%%

