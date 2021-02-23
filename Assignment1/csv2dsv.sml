exception emptyInputFile
exception UnevenFields
exception doubleQuoteError

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
let
	val instream = TextIO.openIn infilename
	val outstream = TextIO.openOut outfilename
	fun process character instream =
		let
			fun convert character instream = 
			let
				fun convert_iter(s,instream,n,control) = 
				case TextIO.input1 instream of
					SOME(c) =>
					if(control = 0) then 
						if(str(c) = "\n") then (s^"\""^"\n",n)
						else if(str(c) = str(delim1)) then convert_iter(s^"\""^str(delim2),instream,n+1,2) 
						else if(str(c) = "\"") then raise doubleQuoteError
						else convert_iter(s^str(c),instream,n,0)
					else if(control = 1) then
						if(str(c) = "\"") then convert_iter(s^"\"",instream,n,3)
						else convert_iter(s^str(c),instream,n,1)
					else if(control = 2) then
						if(str(c) = "\"") then convert_iter(s^"\"",instream,n,1)
						else if(str(c) = str(delim1)) then convert_iter(s^"\""^"\""^str(delim2),instream,n+1,2) 
						else convert_iter(s^"\""^str(c),instream,n,0)
					else
						if(str(c) = "\"") then convert_iter(s^str(c),instream,n,1)
						else if(str(c) = str(delim1)) then convert_iter(s^str(delim2),instream,n+1,2)
						else if(str(c) = "\n") then (s^"\n",n)
						else raise doubleQuoteError
					| NONE => (s,n)
			in
				if(str(character) = "\"") then convert_iter("\"",instream,1,1)
				else convert_iter("\""^str(character),instream,1,0)
			end
		in
			convert character instream
		end;
	fun scanner instream fieldCount lineNo=
		if(fieldCount = ~1) then 
			case TextIO.input1 instream of
				NONE => raise emptyInputFile
				| SOME(character) => 
				let
					val (newLine, fieldCount2) = process character instream
				in 
					TextIO.output(outstream,newLine) before scanner instream fieldCount2 (lineNo+1)
				end
		else
			case TextIO.input1 instream of
				NONE =>  (TextIO.closeOut outstream;TextIO.closeIn instream)
				| SOME(character) => 
				let 
					val (newLine,fieldCount2) = process character instream
				in 
					if(fieldCount2 = fieldCount)
					then TextIO.output(outstream,newLine) before scanner instream fieldCount (lineNo+1)
					else (print("Expected: "^Int.toString(fieldCount)^" fields, Present: "^Int.toString(fieldCount2)^" fields on Line "^
					Int.toString(lineNo));raise UnevenFields)
				end
in	
	scanner instream (~1) 1
end;


fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",",outfilename, #"\t");

fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t",outfilename, #",");



fun convertNewlines(infilename, newline1, outfilename, newline2) = 
let
	val instream = TextIO.openIn infilename
	val outstream = TextIO.openOut outfilename
	fun scanner instream = 2
in
	scanner instream before TextIO.closeIn instream before TextIO.closeOut outstream
end;


fun unix2dos(infilename, outfilename) = convertNewlines(infilename, "\n", outfilename, "\r\n");

fun dos2unix(infilename, outfilename) = convertNewlines(infilename, "\r\n", outfilename, "\n");


