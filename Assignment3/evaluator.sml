structure EVALUATE = 
struct 

open AST

val nonDecl = Fail "Variable not declared"

datatype value = IntVal of int
		| BoolVal of bool
		| LambdaVal of id * exp   

type environment = (id * value) list

fun envLookup(i:id, env:environment) = 
case List.find(fn (x,_) => x = i) env of
 SOME(ex,t) => t
 | NONE     => (print(i);raise nonDecl)
 
fun envAdd(i:id, v:value, env:environment) = (i,v)::env

fun evalExp(e: exp, env:environment) =
case e of
IntExp(i) 			=> (IntVal(i),env)
| ConstExp(i)			=> (if i="true" then BoolVal(true) else BoolVal(false),env)
| VarExp(i)			=> (envLookup(i,env),env)
| ConditionExp(e1,e2,e3)        => let
					  val (v,_) = evalExp(e1,env)
				   in
					case v of
					BoolVal(t) => if t then evalExp(e2,env) else evalExp(e3,env)
				   end
| UnaryExp(u,e1) 		=> let
					val (v,_) = evalExp(e1,env)
				   in
				   	case v of
				   	IntVal(i)    => (IntVal(~1 * i),env)
				   	| BoolVal(i) => (BoolVal(not i),env)
				   end
				   
| BinExp(b,e1,e2)               => evalBinExp(b,e1,e2,env)
| LetExp(ValDecl(i,e1),e2)      => let 
			           	val (v,_) = evalExp(e1,env)
			           in
			           	evalExp(e2,envAdd(i,v,env))
			           end 
| AppExp(e1,e2)			=> evalLambdaExp(e1,e2,env) 
| DeclExp(fd,e1) 		=> case fd of
				   Fn(i,t1,t2) 		=> (LambdaVal(i,e1),env)
				   | Fun(i1,i2,t1,t2)	=> (LambdaVal(i2,e1),envAdd(i1,LambdaVal(i2,e1),env))
and
evalBinExp(b:binop,e1:exp,e2:exp,env:environment) = 
let
	val (v1,_) = evalExp(e1,env)
	val (v2,_) = evalExp(e2,env)	
	
in
	case (b,v1,v2) of
	(Equals,IntVal(i1),IntVal(i2))      		=> (BoolVal(i1 = i2),env)
	| (BoolOp(And),BoolVal(i1),BoolVal(i2))  	=> (BoolVal(i1 andalso i2),env)
	| (BoolOp(Or),BoolVal(i1),BoolVal(i2))  	=> (BoolVal(i1 orelse i2),env)
	| (BoolOp(Xor),BoolVal(i1),BoolVal(i2))  	=> (BoolVal(((not i1) andalso i2) orelse ((not i2) andalso i1)),env)
	| (BoolOp(Implies),BoolVal(i1),BoolVal(i2))    	=> (BoolVal((not i1) orelse i2),env)
	| (IntOp(Add),IntVal(i1),IntVal(i2))       	=> (IntVal(i1+i2),env)
	| (IntOp(Sub),IntVal(i1),IntVal(i2))       	=> (IntVal(i1-i2),env)
	| (IntOp(Times),IntVal(i1),IntVal(i2))       	=> (IntVal(i1*i2),env)
	| (RelOp(LessThan),IntVal(i1),IntVal(i2))       => (BoolVal(i1<i2),env)
	| (RelOp(GreaterThan),IntVal(i1),IntVal(i2))    => (BoolVal(i1>i2),env)
end

and
evalLambdaExp(e1:exp,e2:exp,env:environment) = 
let
	val (v,_) = evalExp(e2,env)
	val (v1,updatedEnv) =
	case e1 of
	DeclExp(Fn(i1,_,_),e3)	=> (LambdaVal(i1,e3),env) 
	| VarExp(i)		=> (envLookup(i,env),env)
	| AppExp(x,y) 		=> (evalLambdaExp(x,y,env))
in
	case v1 of LambdaVal(i,e3) => evalExp(e3,envAdd(i,v,updatedEnv))		   
end


end
	 

