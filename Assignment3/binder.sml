structure A3LrVals = A3LrValsFun(structure Token = LrParser.Token)
structure A3Lex = A3LexFun(structure Tokens = A3LrVals.Tokens);
structure A3Parser =	Join(structure LrParser = LrParser
     	       		structure ParserData = A3LrVals.ParserData
     	       		structure Lex = A3Lex)

open AST
open TYPECHECK 
open EVALUATE
     
fun invoke lexstream =
	let 
		fun print_error (s,rowNum:int,colNum:int) = TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString rowNum) ^ ":" ^ (Int.toString colNum) ^ ":" ^ s ^ "\n")
	in
		A3Parser.parse(0,lexstream,print_error,())
	end


fun fileToLexer fileName =
    	let 
		val instream = TextIO.openIn fileName
		val inputString = TextIO.input instream
		val _ = TextIO.closeIn instream
		val done = ref false
    		val lexer =  A3Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;inputString))
    	in
		lexer
    	end	


fun parse (lexer) =
 	let 
		val dummyEOF = A3LrVals.Tokens.EOF(0,0)
    		val (result, lexer) = invoke lexer
		val (nextToken, lexer) = A3Parser.Stream.get lexer
    	in
        	if A3Parser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    	end


val run = parse o fileToLexer

fun typeCheckList fileName= 
let
	val k = run fileName
	fun checkType([],env:typeEnv) = true
	| checkType(e::pr,env:typeEnv) = 
	let
		val (t,updatedEnv) = typeCheck(e,env)
	in
		checkType(pr,updatedEnv)
	end
in
	case k of
	ExpList(e) => checkType(e,[])
end

fun evaluate fileName=
let 
	val k = run fileName 
	fun evalList([],env:environment) = []
	| evalList(e::pr,env:environment) = 
	let
		val (v,updatedEnv) = evalExp(e,env)
	in
		v::evalList(pr,updatedEnv)
	end
in
	case k of
	ExpList(e) => evalList(e,[])
end
	


