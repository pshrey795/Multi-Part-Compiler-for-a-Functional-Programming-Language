structure Token = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val rowNum = ref 1;
  val colNum = ref 1;
  val firstLine = ref 0;
  val endOfLine = ref 0;
   
  val eof = fn () =>
  let val _ = print("EOF]\n\n")
  in Tokens.EOF(!rowNum, !colNum)
  end
  val error = fn (e, row:int, col:int) => TextIO.output(TextIO.stdOut,"Unknown Token:" ^ (Int.toString row) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")


%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

character=[A-Za-z];
ws = [\ \t ];
%%

"\n"            => (rowNum := !rowNum + 1;endOfLine := yypos;lex());
{ws}+           => (lex());
"NOT"           => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("NOT \"NOT\", ");colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("AND \"AND\", ");colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("OR \"OR\", ");colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"XOR"           => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("XOR \"XOR\", ");colNum := yypos - !(endOfLine);Token.XOR(!rowNum,!colNum));
"EQUALS"        => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("EQUALS \"EQUALS\", ");colNum := yypos - !(endOfLine);Token.EQUALS(!rowNum,!colNum));
"IMPLIES"       => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("IMPLIES \"IMPLIES\", ");colNum := yypos - !(endOfLine);Token.IMPLIES(!rowNum,!colNum));
"IF"            => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("IF \"IF\", ");colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"THEN"          => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("THEN \"THEN\", ");colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"ELSE"          => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("ELSE \"ELSE\", ");colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
";"             => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("TERM \";\", ");colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
"("             => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("LPAREN \"(\", ");colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("RPAREN \")\", ");colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"TRUE"          => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("CONST \"TRUE\", ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
"FALSE"         => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("CONST \"FALSE\", ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
{character}+    => (if (!firstLine)=0 then (print("[");firstLine:=1) else print("");print("ID \""^yytext^"\", ");colNum := yypos - !(endOfLine);Token.ID(yytext,!rowNum,!colNum));
.               => (colNum := yypos - !(endOfLine);error(yytext,!rowNum,!colNum);lex());