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
| BoolExp(i)			=> (BoolVal(i),env)
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
| AppExp(e1,e2)			=> evalAppExp(e1,e2,env) 
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
	| (Equals,BoolVal(i1),BoolVal(i2))      	=> (BoolVal(i1 = i2),env)
	| (Nequals,IntVal(i1),IntVal(i2))      		=> (BoolVal(i1 <> i2),env)
	| (Nequals,BoolVal(i1),BoolVal(i2))      	=> (BoolVal(i1 <> i2),env)
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
evalAppExp(e1:exp,e2:exp,env:environment) =
let
	val (v1,updatedEnv) = evalExp(e1,env)
	val (v,updatedEnv2) = evalExp(e2,updatedEnv)
in
	case v1 of LambdaVal(i,e3) => evalExp(evalLambdaExp(e3,i,v),updatedEnv)	   
end

and
evalLambdaExp(e:exp,i:id,v:value) = 
case e of
VarExp(i1) 			=> if i = i1 then 
				   case v of 
				   	IntVal(n)      		=> IntExp(n)
  					| BoolVal(n)      	=> BoolExp(n)
					| LambdaVal(i2,e1)	=> DeclExp(Fn(i2,PlaceHolder,PlaceHolder),e1)
				   else VarExp(i1)
| ConditionExp(e1,e2,e3)	=> ConditionExp(evalLambdaExp(e1,i,v),evalLambdaExp(e2,i,v),evalLambdaExp(e3,i,v))
| BinExp(b,e1,e2)		=> BinExp(b,evalLambdaExp(e1,i,v),evalLambdaExp(e2,i,v))
| UnaryExp(u,e1)		=> UnaryExp(u,evalLambdaExp(e1,i,v))
| IntExp(n)			=> IntExp(n)
| BoolExp(b)			=> BoolExp(b)
| LetExp(ValDecl(i1,e1),e2)	=> LetExp(ValDecl(i1,evalLambdaExp(e1,i,v)),if i1=i then e2 else evalLambdaExp(e2,i,v)) 
| AppExp(e1,e2)			=> AppExp(evalLambdaExp(e1,i,v),evalLambdaExp(e2,i,v))
| DeclExp(fd,e1)		=> case fd of 
				   	Fn(i1,_,_) 	=> DeclExp(fd,if i=i1 then e1 else evalLambdaExp(e1,i,v))
				   	| Fun(i1,i2,_,_)=> DeclExp(fd,if i=i1 orelse i=i2 then e1 else evalLambdaExp(e1,i,v))
				 
end












	 

