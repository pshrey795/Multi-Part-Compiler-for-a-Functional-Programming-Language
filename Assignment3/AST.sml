structure AST =
struct

type id = string

datatype typ = INT | BOOL | Arrow of typ * typ | PlaceHolder     

datatype binop1 = And | Or | Xor | Implies
datatype binop2 = Add | Sub | Times
datatype binop3 = GreaterThan | LessThan 
datatype unop = Not | Negate

datatype binop = BoolOp of binop1
                | IntOp of binop2
                | RelOp of binop3
                | Equals
                | Nequals

datatype decl = ValDecl of id * exp  

and fundecl = Fun of id * id * typ * typ                       
	      | Fn of id * typ * typ

and exp = ConditionExp of exp * exp * exp    
        | BinExp of binop * exp * exp		
        | UnaryExp of unop * exp
        | VarExp of id
        | BoolExp of bool
        | LetExp of decl * exp
        | IntExp of int
        | AppExp of exp * exp 
        | DeclExp of fundecl * exp       
        
datatype program = ExpList of exp list

fun addExp(e:exp, pr:program) = case pr of ExpList(ls) => ExpList(e::ls)

end


