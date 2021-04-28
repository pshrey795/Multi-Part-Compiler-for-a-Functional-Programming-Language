%%

%name A3

%term
  ID of string | CONST of string | NUM of int | INT | BOOL | COLON | FUN | FN | DEC | ARROW | NOT | TERM | AND | OR | XOR | EQUALS | NEGATE | PLUS | MINUS | TIMES | GREATERTHAN | LESSTHAN | IMPLIES | LET | IN | END | IF | THEN | ELSE | FI | ASSN | RPAREN | LPAREN | EOF 

%nonterm FILE of AST.program | PROGRAM of AST.program | EXP of AST.exp | DECL of AST.decl | FUNDECL of AST.fundecl | TYP of AST.typ | VAR of AST.exp

%pos int

%eop EOF
%noshift EOF

%start FILE

(* Defining associativity rules *)
%right ARROW
%right DEC
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%nonassoc GREATERTHAN LESSTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE
%nonassoc ID NUM CONST LPAREN


%verbose

%%

(* Production Rules along with printing them *)

FILE: PROGRAM (PROGRAM)

PROGRAM: EXP TERM PROGRAM (AST.addExp(EXP,PROGRAM)) | EXP (AST.addExp(EXP,AST.ExpList([]))) | (AST.ExpList([]))   

EXP: IF EXP THEN EXP ELSE EXP FI(AST.ConditionExp(EXP1,EXP2,EXP3)) 
    | LET DECL IN EXP END(AST.LetExp(DECL,EXP)) 
    | VAR (VAR)
    | EXP VAR (AST.AppExp(EXP,VAR))
    | EXP AND EXP (AST.BinExp(AST.BoolOp(AST.And),EXP1,EXP2))  
    | EXP OR EXP (AST.BinExp(AST.BoolOp(AST.Or),EXP1,EXP2))
    | EXP XOR EXP (AST.BinExp(AST.BoolOp(AST.Xor),EXP1,EXP2))
    | EXP EQUALS EXP (AST.BinExp(AST.Equals,EXP1,EXP2))
    | EXP IMPLIES EXP (AST.BinExp(AST.BoolOp(AST.Implies),EXP1,EXP2))
    | EXP PLUS EXP (AST.BinExp(AST.IntOp(AST.Add),EXP1,EXP2))
    | EXP MINUS EXP (AST.BinExp(AST.IntOp(AST.Sub),EXP1,EXP2))
    | EXP TIMES EXP (AST.BinExp(AST.IntOp(AST.Times),EXP1,EXP2))
    | EXP GREATERTHAN EXP (AST.BinExp(AST.RelOp(AST.GreaterThan),EXP1,EXP2))
    | EXP LESSTHAN EXP (AST.BinExp(AST.RelOp(AST.LessThan),EXP1,EXP2))
    | FUNDECL DEC EXP (AST.DeclExp(FUNDECL,EXP))
    
VAR: ID (AST.VarExp(ID))
     | CONST (AST.ConstExp(CONST)) 
     | NUM (AST.IntExp(NUM))
     | NEGATE VAR(AST.UnaryExp(AST.Negate,VAR))
     | NOT VAR(AST.UnaryExp(AST.Not,VAR))
     | LPAREN EXP RPAREN(EXP)  
    
FUNDECL: FUN ID LPAREN ID COLON TYP RPAREN COLON TYP (AST.Fun(ID1,ID2,TYP1,TYP2))
	| FN LPAREN ID COLON TYP RPAREN COLON TYP (AST.Fn(ID1,TYP1,TYP2))
	
TYP: INT (AST.INT)
    | BOOL (AST.BOOL)
    | TYP ARROW TYP (AST.Arrow(TYP1,TYP2))
    | LPAREN TYP RPAREN ARROW TYP(AST.Arrow(TYP1,TYP2))  

DECL: ID ASSN EXP(AST.ValDecl(ID,EXP))
