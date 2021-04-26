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

fun typeCheck(e:exp, env:typeEnv) = 
case e of 
  IntExp(i) 			=> (INT,env)
| ConstExp(i) 			=> (BOOL,env)
| VarExp(i)			=> (typeEnvLookup(i,env),env)
| ConditionExp(e1,e2,e3)	=> let val (t1,_) = typeCheck(e1,env)
				   in 
				   	if t1=BOOL then 
				   	let
				   		val (t2,_) = typeCheck(e2,env)
				   		val (t3,_) = typeCheck(e3,env)
				   	in
				   		if t2=t3 then (t2,env) else raise invalidType
				   	end
				   	else raise invalidType
				   end
| UnaryExp(u,e1)		=> let val (t,_) = typeCheck(e1,env)
				   in case u of
				    Not     => if t=BOOL then (BOOL,env) else raise invalidType
				   |Negate => if t=INT then (INT,env) else raise invalidType
				   end
| BinExp(b,e1,e2)		=> let 
				   	val (t1,_) = typeCheck(e1,env)
				   	val (t2,_) = typeCheck(e2,env)
				   in
				   	if t1<>t2 then raise invalidType
				   	else case b of
				   	  BoolOp(x)   => if t1=BOOL then (BOOL,env) else raise invalidType
				   	| IntOp(x)    => if t1=INT then (INT,env) else raise invalidType
				   	| RelOp(x)    => if t1=INT then (BOOL,env) else raise invalidType
				   	| Equals      => (BOOL,env)
				   end 
| LetExp(ValDecl(i,e1),e2)      => typeCheck(e2,typeEnvAdd(i,let val (t,_) = typeCheck(e1,env) in t end,env))
| AppExp(e1,e2)                 => let 
				   	val (t1,_) = typeCheck(e1,env)
				   	val (t2,_) = typeCheck(e2,env)
				   in 
				   	case t1 of
					Arrow(t,v) => if t2=t then (v,env) else raise invalidType
					| _         => raise invalidType
				   end	
| DeclExp(fd,e1)		=> case fd of
				   Fn(i,t1,t2)	      => let
				   		   		val (t,_) = typeCheck(e1,typeEnvAdd(i,t1,env))
				   		   	 in
				   		   		if t=t2 then (Arrow(t1,t2),env) else raise invalidType
				   		   	 end
				   | Fun(i1,i2,t1,t2) => let
				   				val env = typeEnvAdd(i1,Arrow(t1,t2),env)
				   		   		val (t,_) = typeCheck(e1,typeEnvAdd(i2,t1,env))
				   		   	 in
				   		   		if t=t2 then (Arrow(t1,t2),env) else raise invalidType
				   		   	 end	   		   	 
				   		   	
				   
end
