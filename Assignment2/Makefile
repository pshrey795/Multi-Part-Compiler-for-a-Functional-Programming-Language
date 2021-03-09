 all: A2.lex.sml A2.yacc.*
	mlton binder.sml

A2.lex.sml: A2.lex
	ml-lex A2.lex

A2.yacc.*: A2.yacc
	ml-yacc A2.yacc

clean:
	rm A2.lex.* A2.yacc.* 