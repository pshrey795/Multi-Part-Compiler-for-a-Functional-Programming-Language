
%%
(* required declarations *)
%name A2

%term
  ID of string | CONST of string | NOT | TERM | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | RPAREN | LPAREN | EOF 

%nonterm program | statement | formula | file | conditional | term | expression | factor | binOp

%pos int

%eop EOF
%noshift EOF

%start file

%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%verbose

%%

file: program ()
program: statement program ()
statement: formula TERM ()
formula: expression () | conditional ()
conditional: IF formula THEN formula ELSE formula ()
expression: term IMPLIES expression () | term IMPLIES conditional () | term ()
term: term AND factor () | term AND conditional () | factor ()
factor: CONST ()| ID ()| NOT factor ()| LPAREN factor RPAREN () | NOT conditional()