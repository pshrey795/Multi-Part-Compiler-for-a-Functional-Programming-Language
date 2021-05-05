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
   
  
  val eof = fn () => Tokens.EOF(!rowNum, !colNum) 

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
"NOT"           	=> (colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           	=> (colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            	=> (colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"XOR"           	=> (colNum := yypos - !(endOfLine);Token.XOR(!rowNum,!colNum));
"EQUALS"      	  	=> (colNum := yypos - !(endOfLine);Token.EQUALS(!rowNum,!colNum));
"NEGATE"        	=> (colNum := yypos - !(endOfLine);Token.NEGATE(!rowNum,!colNum));
"PLUS"          	=> (colNum := yypos - !(endOfLine);Token.PLUS(!rowNum,!colNum));
"MINUS"         	=> (colNum := yypos - !(endOfLine);Token.MINUS(!rowNum,!colNum));
"TIMES"         	=> (colNum := yypos - !(endOfLine);Token.TIMES(!rowNum,!colNum));
"GREATERTHAN"   	=> (colNum := yypos - !(endOfLine);Token.GREATERTHAN(!rowNum,!colNum));
"LESSTHAN"      	=> (colNum := yypos - !(endOfLine);Token.LESSTHAN(!rowNum,!colNum));
"IMPLIES"       	=> (colNum := yypos - !(endOfLine);Token.IMPLIES(!rowNum,!colNum));
"let"           	=> (colNum := yypos - !(endOfLine);Token.LET(!rowNum,!colNum));
"in"            	=> (colNum := yypos - !(endOfLine);Token.IN(!rowNum,!colNum));
"end"          	 	=> (colNum := yypos - !(endOfLine);Token.END(!rowNum,!colNum));
"if"            	=> (colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"then"          	=> (colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"fi"            	=> (colNum := yypos - !(endOfLine);Token.FI(!rowNum,!colNum));
"else"          	=> (colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
"fun"           	=> (colNum := yypos - !(endOfLine);Token.FUN(!rowNum,!colNum));
"fn"            	=> (colNum := yypos - !(endOfLine);Token.FN(!rowNum,!colNum));
"int"            	=> (colNum := yypos - !(endOfLine);Token.INT(!rowNum,!colNum));
"bool"            	=> (colNum := yypos - !(endOfLine);Token.BOOL(!rowNum,!colNum));
";"             	=> (colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
":"             	=> (colNum := yypos - !(endOfLine);Token.COLON(!rowNum,!colNum));
"("             	=> (colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             	=> (colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"="             	=> (colNum := yypos - !(endOfLine);Token.ASSN(!rowNum,!colNum));
"->"			=> (colNum := yypos - !(endOfLine);Token.ARROW(!rowNum,!colNum)); 
"=>"			=> (colNum := yypos - !(endOfLine);Token.DEC(!rowNum,!colNum));   
"NEQUALS"		=> (colNum := yypos - !(endOfLine);Token.NEQUALS(!rowNum,!colNum));
"TRUE"          	=> (colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
"FALSE"         	=> (colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
[a-zA-z]{variable}*    	=> (colNum := yypos - !(endOfLine);Token.ID(yytext,!rowNum,!colNum));
{digit}+        	=> (colNum := yypos - !(endOfLine);Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!rowNum, !colNum));
.               	=> (colNum := yypos - !(endOfLine)-size yytext;error(yytext,!rowNum,!colNum);raise TokenError);
