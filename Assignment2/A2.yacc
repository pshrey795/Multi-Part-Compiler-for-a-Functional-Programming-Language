%%

%name A2

%term
  ID of string | CONST of string | NOT | TERM | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | RPAREN | LPAREN | EOF 

%nonterm program | statement | formula | file | Expression | Term | Factor | binOp

%pos int

%eop EOF
%noshift EOF

%start file

(* Defining associativity rules *)
%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT


%verbose

%%

(* Production Rules along with printing them *)
file: program (print("file => program"))
program: statement program (print("program => {statement}")) | ()
statement: formula TERM (print("statement => formula TERM\n"))
formula: IF formula THEN formula ELSE formula (print("formula => IF formula THEN formula ELSE formula\n")) | Expression (print("formula => Expression\n"))
Expression: Term IMPLIES Expression (print("Expression => Term IMPLIES Expression\n")) | Term (print("Expression => Term\n"))
Term: Term binOp Factor (print("Term => Term binOp Factor\n")) | Factor(print("Term => Factor\n"))
Factor: NOT Factor(print("Factor => NOT Factor\n")) | LPAREN formula RPAREN (print("Factor => LPAREN formula RPAREN\n")) | CONST (print("Factor => CONST\n")) | ID (print("Factor => ID\n"))
binOp: AND (print("binOp => AND\n"))| OR (print("binOp => OR\n"))| XOR (print("binOp => XOR\n"))| EQUALS (print("binOp => EQUALS\n"))
