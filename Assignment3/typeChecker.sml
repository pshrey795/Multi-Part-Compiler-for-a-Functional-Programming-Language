structure TYPECHECK = 
struct 
open AST

val invalidType = Fail "Type error"
val nonDecl = Fail "Variable not declared"

type typeEnv = (id * typ) list

fun typeEnvLookup(i: id, env:typeEnv) = 
case List.find(fn (x,_) => x = i) env of
 SOME(ex,t) => t
 | NONE     => (print(i);raise nonDecl)

fun typeEnvAdd(i:id, t: typ, env:typeEnv) = (i,t)::env

fun typeCheck(e:exp, env:typeEnv,expNo:int) = 
case e of 
  IntExp(i) 			=> (INT,env)
| BoolExp(i) 			=> (BOOL,env)
| VarExp(i)			=> (typeEnvLookup(i,env),env)
| ConditionExp(e1,e2,e3)	=> let val (t1,_) = typeCheck(e1,env,expNo)
				   in 
				   	if t1=BOOL then 
				   	let
				   		val (t2,_) = typeCheck(e2,env,expNo)
				   		val (t3,_) = typeCheck(e3,env,expNo)
				   	in
				   		if t2=t3 then (t2,env) 
				   		else (print("Then and else clause are of different types in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   	end
				   	else (print("If clause should have bool type in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   end
| UnaryExp(u,e1)		=> let val (t,_) = typeCheck(e1,env,expNo)
				   in case u of
				    Not     => if t=BOOL then (BOOL,env) 
				    else (print("Bool type expected with NOT operator in expression number "^Int.toString(expNo)^"\n");raise invalidType) 
				   |Negate => if t=INT then (INT,env) 
				    else (print("Int type expected with NEGATE operator in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   end
| BinExp(b,e1,e2)		=> let 
				   	val (t1,_) = typeCheck(e1,env,expNo)
				   	val (t2,_) = typeCheck(e2,env,expNo)
				   in
				   	if t1<>t2 then 
				   	(print("Both operands are expected to be of same type in binary expression "^Int.toString(expNo)^"\n");raise invalidType)
				   	else case b of
				   	  BoolOp(x)   => if t1=BOOL then (BOOL,env) 
				   	  else (print("Both operands should be of bool type with a boolean operator in the expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   	| IntOp(x)    => if t1=INT then (INT,env) 
				   	  else (print("Both operands should be of int type with an arithmetic operator in the expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   	| RelOp(x)    => if t1=INT then (BOOL,env) 
				   	  else (print("Both operands should be of int type with a relation operator in the expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   	| Equals      => (BOOL,env)
				   	| Nequals     => (BOOL,env)
				   end 
| LetExp(ValDecl(i,e1),e2)      => let
					val (t,_) = typeCheck(e2,typeEnvAdd(i,let val (t,_) = typeCheck(e1,env,expNo) in t end,env),expNo)
				   in
				   	(t,env)
				   end
| AppExp(e1,e2)                 => let 
				   	val (t1,env1) = typeCheck(e1,env,expNo)
				   	val (t2,_) = typeCheck(e2,env1,expNo)
				   in 
				   	case t1 of
					Arrow(t,v) => if t2=t then (v,env) 
					else (print("The type of operand doesn't match with the domain of the function in expression number "^Int.toString(expNo)^"\n");raise invalidType)
					| _  => (print("The type of operand doesn't match with the domain of the function in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   end	
| DeclExp(fd,e1)		=> case fd of
				   Fn(i,t1,t2)	      => let
				   		   		val (t,_) = typeCheck(e1,typeEnvAdd(i,t1,env),expNo)
				   		   	 in
				   		   		if t=t2 then (Arrow(t1,t2),env) 
				   		   		else (print("The type of function definition doesn't match with the co-domain of the function in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   		   	 end
				   | Fun(i1,i2,t1,t2) => let
				   				val env = typeEnvAdd(i1,Arrow(t1,t2),env)
				   		   		val (t,_) = typeCheck(e1,typeEnvAdd(i2,t1,env),expNo)
				   		   	 in
				   		   		if t=t2 then (Arrow(t1,t2),env) 
				   		   		else (print("The type of function definition doesn't match with the co-domain of the function in expression number "^Int.toString(expNo)^"\n");raise invalidType)
				   		   	 end	   		   	 
				   		   	
				   
end
