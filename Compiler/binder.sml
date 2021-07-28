structure CompLrVals = CompLrValsFun(structure Token = LrParser.Token)
structure CompLex = CompLexFun(structure Tokens = CompLrVals.Tokens);
structure CompParser =	Join(structure LrParser = LrParser
     	       		structure ParserData = CompLrVals.ParserData
     	       		structure Lex = CompLex)

open AST
open TYPECHECK 
open EVALUATE
     
fun invoke lexstream =
	let 
		fun print_error (s,rowNum:int,colNum:int) = TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString rowNum) ^ ":" ^ (Int.toString colNum) ^ ":" ^ s ^ "\n")
	in
		CompParser.parse(0,lexstream,print_error,())
	end


fun fileToLexer fileName =
    	let 
		val instream = TextIO.openIn fileName
		val inputString = TextIO.input instream
		val _ = TextIO.closeIn instream
		val done = ref false
    		val lexer =  CompParser.makeLexer (fn _ => if (!done) then "" else (done:=true;inputString))
    	in
		lexer
    	end	


fun parse (lexer) =
 	let 
		val dummyEOF = CompLrVals.Tokens.EOF(0,0)
    		val (result, lexer) = invoke lexer
		val (nextToken, lexer) = CompParser.Stream.get lexer
    	in
        	if CompParser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    	end


val run = parse o fileToLexer

fun checkType fileName= 
let
	val k = run fileName
	fun typeCheckList([],env:typeEnv,expNo:int) = true
	| typeCheckList(e::pr,env:typeEnv,expNo) = 
	let
		val (t,updatedEnv) = typeCheck(e,env,expNo)
	in
		typeCheckList(pr,updatedEnv,expNo+1)
	end
in
	case k of
	ExpList(e) => typeCheckList(e,[],1)
end

fun evaluate fileName=
let 
	val k = run fileName 
	fun evalList([],env:environment) = []
	| evalList(e::pr,env:environment) = 
	let
		val (v,updatedEnv) = evalExp(e,env)
	in
		case v of 
		LambdaVal(i,e) 	=> evalList(pr,updatedEnv)
		| _		=> v::evalList(pr,updatedEnv) 
	end
in
	if checkType fileName then case k of
	ExpList(e) => evalList(e,[])
	else []
end
	


