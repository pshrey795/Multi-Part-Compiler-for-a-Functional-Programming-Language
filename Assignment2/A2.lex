structure Token = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val rowNum = ref 1;
  val colNum = ref 1;
  val endOfLine = ref 0;
  val eof = fn () => Tokens.EOF(!rowNum, !colNum)
  val error = fn (e, row:int, col:int) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString row) ^ " " ^ (Int.toString col) ^ ": " ^ e ^ "\n")

%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

character=[A-Za-z];
ws = [\ \t ];
%%

"\n"            => (rowNum := !rowNum + 1;endOfLine := yypos;lex());
{ws}+           => (lex());
"NOT"           => (print("NOT ");colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           => (print("AND ");colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            => (print("OR ");colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"XOR"           => (print("XOR ");colNum := yypos - !(endOfLine);Token.XOR(!rowNum,!colNum));
"EQUALS"        => (print("EQUALS ");colNum := yypos - !(endOfLine);Token.EQUALS(!rowNum,!colNum));
"IMPLIES"       => (print("IMPLIES ");colNum := yypos - !(endOfLine);Token.IMPLIES(!rowNum,!colNum));
"IF"            => (print("IF ");colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"THEN"          => (print("THEN ");colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"ELSE"          => (print("ELSE ");colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
";"             => (print(";\n");colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
"("             => (print("(");colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             => (print(")");colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"TRUE"          => (print("TRUE ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
"FALSE"         => (print("FALSE ");colNum := yypos - !(endOfLine);Token.CONST(yytext,!rowNum,!colNum));
{character}+    => (print(yytext^" ");colNum := yypos - !(endOfLine);Token.ID(yytext,!rowNum,!colNum));
.               => (colNum := yypos - !(endOfLine);error(yytext,!rowNum,!colNum);lex());