structure Token = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

character=[A-Za-z];
ws = [\ \t ];
%%

"\n"            => (pos := !pos + 1;lex());
{ws}+           => (lex());
"NOT"           => (Token.NOT(!pos,!pos));
"AND"           => (Token.AND(!pos,!pos));
"OR"            => (Token.OR(!pos,!pos));
"XOR"           => (Token.XOR(!pos,!pos));
"EQUALS"        => (Token.EQUALS(!pos,!pos));
"IMPLIES"       => (Token.IMPLIES(!pos,!pos));
"IF"            => (Token.IF(!pos,!pos));
"THEN"          => (Token.THEN(!pos,!pos));
"ELSE"          => (Token.ELSE(!pos,!pos));
";"             => (Token.TERM(!pos,!pos));
"("             => (Token.LPAREN(!pos,!pos));
")"             => (Token.RPAREN(!pos,!pos));
"TRUE"          => (Token.CONST(yytext,!pos,!pos));
"FALSE"         => (Token.CONST(yytext,!pos,!pos));
{character}+    => (Token.ID(yytext,!pos,!pos));
.               => (error ("ignoring bad character "^yytext,!pos,!pos);lex());