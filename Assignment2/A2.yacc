
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
program: statement program () | ()
statement: formula TERM ()
formula: IF formula THEN formula ELSE formula ()
        | formula IMPLIES formula ()
        | formula AND formula ()
        | formula OR formula ()
        | formula XOR formula ()
        | formula EQUALS formula ()
        | NOT formula ()
        | LPAREN formula RPAREN ()
        | ID ()
        | CONST ()