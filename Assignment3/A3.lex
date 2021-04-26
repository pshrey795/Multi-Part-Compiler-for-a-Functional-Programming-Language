structure Token = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  exception TokenError

  val rowNum = ref 1;
  val colNum = ref 1;
  val firstLine = ref 0;
  val endOfLine = ref 0;
   
  
  val eof = fn () =>
  let val _ = print("EOF\n\n")
  in Tokens.EOF(!rowNum, !colNum)
  end

  val error = fn (e, row:int, col:int) => TextIO.output(TextIO.stdOut,"Unknown Token:" ^ (Int.toString row) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")


%%

%header (functor A3LexFun(structure Tokens:A3_TOKENS));

digit=[0-9];
variable=[A-Za-z0-9];
ws = [\ \t ];
newline = [\r \n \r\n];

%%

{newline}       	=> (rowNum := !rowNum + 1;endOfLine := yypos;lex());
{ws}+           	=> (lex());
"NOT"           	=> (print("NOT \"NOT\", ");colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           	=> (print("AND \"AND\", ");colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            	=> (print("OR \"OR\", ");colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"XOR"           	=> (print("XOR \"XOR\", ");colNum := yypos - !(endOfLine);Token.XOR(!rowNum,!colNum));
"EQUALS"      	  	=> (print("EQUALS \"EQUALS\", ");colNum := yypos - !(endOfLine);Token.EQUALS(!rowNum,!colNum));
"NEGATE"        	=> (print("NEGATE \"NEGATE\", ");colNum := yypos - !(endOfLine);Token.NEGATE(!rowNum,!colNum));
"PLUS"          	=> (print("PLUS \"PLUS\", ");colNum := yypos - !(endOfLine);Token.PLUS(!rowNum,!colNum));
"MINUS"         	=> (print("MINUS \"MINUS\", ");colNum := yypos - !(endOfLine);Token.MINUS(!rowNum,!colNum));
"TIMES"         	=> (print("TIMES \"TIMES\", ");colNum := yypos - !(endOfLine);Token.TIMES(!rowNum,!colNum));
"GREATERTHAN"   	=> (print("GREATERTHAN \"GREATERTHAN\", ");colNum := yypos - !(endOfLine);Token.GREATERTHAN(!rowNum,!colNum));
"LESSTHAN"      	=> (print("LESSTHAN \"LESSTHAN\", ");colNum := yypos - !(endOfLine);Token.LESSTHAN(!rowNum,!colNum));
"IMPLIES"       	=> (print("IMPLIES \"IMPLIES\", ");colNum := yypos - !(endOfLine);Token.IMPLIES(!rowNum,!colNum));
"let"           	=> (print("LET \"let\", ");colNum := yypos - !(endOfLine);Token.LET(!rowNum,!colNum));
"in"            	=> (print("IN \"in\", ");colNum := yypos - !(endOfLine);Token.IN(!rowNum,!colNum));
"end"          	 	=> (print("END \"end\", ");colNum := yypos - !(endOfLine);Token.END(!rowNum,!colNum));
"if"            	=> (print("IF \"if\", ");colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"then"          	=> (print("THEN \"then\", ");colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"fi"            	=> (print("FI \"fi\", ");colNum := yypos - !(endOfLine);Token.FI(!rowNum,!colNum));
"else"          	=> (print("ELSE \"else\", ");colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
"fun"           	=> (print("FUN \"fun\", ");colNum := yypos - !(endOfLine);Token.FUN(!rowNum,!colNum));
"fn"            	=> (print("FN \"fn\", ");colNum := yypos - !(endOfLine);Token.FN(!rowNum,!colNum));
"int"            	=> (print("INT \"int\", ");colNum := yypos - !(endOfLine);Token.INT(!rowNum,!colNum));
"bool"            	=> (print("BOOL \"bool\", ");colNum := yypos - !(endOfLine);Token.BOOL(!rowNum,!colNum));
";"             	=> (print("TERM \";\", ");colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
":"             	=> (print("COLON \":\", ");colNum := yypos - !(endOfLine);Token.COLON(!rowNum,!colNum));
"("             	=> (print("LPAREN \"(\", ");colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             	=> (print("RPAREN \")\", ");colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"="             	=> (print("ASSN \"=\", ");colNum := yypos - !(endOfLine);Token.ASSN(!rowNum,!colNum));
"->"			=> (print("ARROW \"->\", ");colNum := yypos - !(endOfLine);Token.ARROW(!rowNum,!colNum));
"=>"			=> (print("DEC \"=>\", ");colNum := yypos - !(endOfLine);Token.DEC(!rowNum,!colNum));
"TRUE"          	=> (print("CONST \"TRUE\", ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
"FALSE"         	=> (print("CONST \"FALSE\", ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
[a-zA-z]{variable}*    	=> (print("ID \""^yytext^"\", ");colNum := yypos - !(endOfLine);Token.ID(yytext,!rowNum,!colNum));
{digit}+        	=> (print("NUM \""^yytext^"\", ");colNum := yypos - !(endOfLine);Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!rowNum, !colNum));
.               	=> (colNum := yypos - !(endOfLine)-size yytext;error(yytext,!rowNum,!colNum);raise TokenError);
