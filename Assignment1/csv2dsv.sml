(*This file has been prepared by Shrey J. Patel 2019CS10400 *)

(* Exceptions, note that their type is self-explanatory from their name *)
exception emptyInputFile		
exception UnevenFields of string
exception doubleQuoteError
exception invalidNewlineUsage


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
let
	(* Using TextIO for input/output *)
	val instream = TextIO.openIn infilename
	val outstream = TextIO.openOut outfilename
	
	(* This function processes a given line starting with the char "character" and raises appropriate exceptions for 
	semantic errors *)
	fun process character instream =
		(* Parameters: character: first character of current line, instream: Input stream *)
		
		let
		
			(* Auxiliary function to process the line character by character *)
			(* This function converts unescaped delimiters, terminates when encountered with an unescaped newline
			character and ignores escaped delimeters and newline characters. It raises exceptions on incorrect
			usage of double quotes *)
			fun convert(instream,n,control) = 
				(*Parameters: instream: Input stream, n:current number of fields, control: this is an integer 
				representing the various states of string processing, which decides how to process the current 
				character*)
				
				(* Processing the next character using TextIO.input *)
				case TextIO.input1 instream of
				
					(* Case structure to decide how to process the input character depending upon the 
					current state of function(decided by the parameter "control") and on the character 
					itself *) 
					SOME(c) =>
					
					(* Control structure and the various states it defines:
					
					control=0 => The current field is not enclosed within double quotes, meaning any  
						     delimeter or newline should not be escaped.
					control=1 => The current field is enclosed within double quotes and so any
						     delimiter or newline character should be escaped. 
					control=2 => Transitions from control=0 as soon as a delimiter is encountered to denote
						     the starting of a new field. Transitions back to control=2, control=1 or 
						     control=0 depending on whether the next character is a delimiter, double 
						     quote or some other character.
					control=3 => Transitions from control=2 as soon as a double quote is encountered to 
						     denote the ending of the current field, or an escaped double quote inside
						     the current field. Transitions to control=1 or control=2 depending upon
						     one of the above two cases.
						     
					Note that the if-then-else blocks below process/convert the characters differently
					depending on the current value of control.	     	   
					*)
					if(control = 0) then 
						(* Detects the end of line(unescaped) *)
						if(str(c) = "\n") then (TextIO.output(outstream,"\""^"\n");n)
						(* Switching delimiters, and increasing the count of fields processed *)
						else if(str(c) = str(delim1)) 
						then (TextIO.output(outstream,"\""^str(delim2));convert(instream,n+1,2)) 
						(* Single(unescaped) double quote raises an exception *)
						else if(str(c) = "\"") then raise doubleQuoteError
						(* Normal character, other than newlines or delimiters *)
						else (TextIO.output(outstream,str(c));convert(instream,n,0))
					else if(control = 1) then
						(* Detects either the end of field double quote or an escaped double quote *)
						if(str(c) = "\"") then (TextIO.output(outstream,"\"");convert(instream,n,3))
						(* Rest of the characters are processed without any change, including newlines
						and delimiters as they have been escaped *)
						else (TextIO.output(outstream,str(c));convert(instream,n,1))
					else if(control = 2) then
						(* Starting of the next field with a double quote*)
						if(str(c) = "\"") then (TextIO.output(outstream,"\"");convert(instream,n,1))
						(* Detects a delimeter followed by a delimeter i.e. without any field, this is 
						to be interpreted as an empty field *)
						else if(str(c) = str(delim1)) then
						(TextIO.output(outstream,"\""^"\""^str(delim2));convert(instream,n+1,2))
						(* Detects a delimeter followed by a new line i.e. without any field, this is
						to be interpreted as an empty field *)     
						else if(str(c) = "\n") then (TextIO.output(outstream,"\"\"\n");n)
						(* Starting of the next field without a double quote*)
						else (TextIO.output(outstream,"\""^str(c));convert(instream,n,0))
					else
						(* Detects an escaped double quote *)
						if(str(c) = "\"") then (TextIO.output(outstream,"\"");convert(instream,n,1))
						(* Detects a double quote signifying an end of the field, and  *)
						else if(str(c) = str(delim1)) then 
						(TextIO.output(outstream,str(delim2));convert(instream,n+1,2))
						(* Detects the end of line *)
						else if(str(c) = "\n") then (TextIO.output(outstream,"\n");n)
						(* If a single(unescaped) double quote is followed by any character other than
						the ones listed above, then an exception is raised *)
						else raise doubleQuoteError
					| NONE => raise invalidNewlineUsage (* Stops processing input at the end of file *)
		in
		
			(* Processing the first character of the current line i.e. data record *)
			if(str(character) = "\n") then (TextIO.output(outstream,"\"\"\n");0)
			else if(str(character) = "\"") then (TextIO.output(outstream,"\"");convert(instream,1,1))
			else (TextIO.output(outstream,"\""^str(character));convert(instream,1,0))
		end;
		
	(* Primary scanner function which scans the input file line by line *)
	fun scanner instream fieldCount lineNo=
		(* Parameters: instream: Input stream, fieldCount: no of fields of current line, lineNo: current line number *)
		
		(* Checking if the current line of the input file is the first line or not *)
		if(lineNo = 1) then 
		
			(* If first line, then store the number of fields of this line in variable fieldCount2 and pass onto
			subsequent scanner calls for checking if the number of fields is same in each data record *) 
			case TextIO.input1 instream of
				NONE => raise emptyInputFile
				| SOME(character) => 
				let
					(* newLine=processed line, fieldCount2=no of fields of current line *) 
					val fieldCount2 = process character instream
				in 
					(* Processing subsequent lines *)
					scanner instream fieldCount2 (lineNo+1)
				end
		
		else
			
			(* If not the first line, then after processing the said line and storing the number of fields in 
			fieldCount2, check if it is equal to the number of fields of first line and raise an exception
			with appropriate comments if not found equal *)			
			case TextIO.input1 instream of
				NONE =>  (TextIO.closeOut outstream;TextIO.closeIn instream)
				| SOME(character) => 
				let 
					(* newLine=processed line, fieldCount2=no of fields of current line *)
					val fieldCount2 = process character instream
				in
					(* Checking if the current line has the correct number of fields *) 
					if(fieldCount2 = fieldCount)
					(* If correct number of fields, then process subsequent lines *)
					then scanner instream fieldCount (lineNo+1)
					(* If incorrect, then raise and handle an exception with a custom string as below *)
					else raise UnevenFields("Expected: "^Int.toString(fieldCount)^" fields, Present: "^Int.toString(fieldCount2)^" fields on Line "^Int.toString(lineNo))
				end
				handle UnevenFields(s) => print(s^"\n")
in	
	scanner instream 0 1
end;

(* Converting comma separated file to tab separated file *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",",outfilename, #"\t");

(* Converting tab separated file to comma separated file *)
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t",outfilename, #",");
