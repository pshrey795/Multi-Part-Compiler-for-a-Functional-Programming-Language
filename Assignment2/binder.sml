structure A2LrVals = A2LrValsFun(structure Token = LrParser.Token)
structure A2Lex = A2LexFun(structure Tokens = A2LrVals.Tokens);
structure A2Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = A2LrVals.ParserData
     	       structure Lex = A2Lex)
     
fun invoke lexstream =
    let 
		fun print_error (s,rowNum:int,colNum:int) =
		TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString rowNum) ^ ":" ^ (Int.toString colNum) ^ ":" ^ s ^ "\n")
	in
		A2Parser.parse(0,lexstream,print_error,())
	end

fun fileToLexer fileName =
    let 
		val instream = TextIO.openIn fileName
		val inputString = TextIO.input instream
		val _ = TextIO.closeIn instream
		val done = ref false
    	val lexer =  A2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;inputString))
    in
		lexer
    end	

fun parse (lexer) =
    let 
		val dummyEOF = A2LrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = A2Parser.Stream.get lexer
    in
        if A2Parser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o fileToLexer




