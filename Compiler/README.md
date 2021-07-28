Stage 2: Fully Functional Multi-Stage Compiler
===

Compiler for a functional programming language (subset of SML). The exact problem statements can be found in either [Statement1](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/Compiler/ProblemStatement/Statement1.pdf) or [Statement2](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/Compiler/ProblemStatement/Statement2.pdf) . The language has been created from a context-free grammar specification in [**Extended Backus Naur Form(EBNF)**](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/tree/master/Compiler/EBNF.txt). The compiler has been created in the following different parts:

1. Lexer/Scanner using ML-Lex
2. Parser using ML-Yacc
3. Type Checker using SML
4. Evaluator using SML
5. Binder/Glue code
6. Loader

How to run the files:
---

* Clone the directory using `git clone  https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/Compiler`
* Run the `make` command in the terminal, which compiles all the files, runs the loader which loads all the pre-requisite routines, runs the binder which joins all the files and finally opens the SML interactive environment. 

1. **Lexer:**
 * To run the lexer inside SML, run the command `fileToLexer <path of input file>`
 * This outputs a list of comma separated fields, where each field is of the type "<TOKEN_TYPE> <TOKEN_NAME>"
 * In case an invalid token is encountered, the lexer will throw an error of the form "Unknown Token:<ROW_NUM>:<col_num>:<token>

2. **Parser:**
 * The parser can be invoked by using the command `run <path of input file>`
 * Alternatively, you can also use the command `parse o fileToLexer <path of input file>`
 * The parser returns a parse tree, in a recursive form,  in which the terminal nodes represent tokens and the internal nodes represent the non-terminals.
 * In case of a parse error, it raises an error of the type "Syntax Error:<ROW_NUM>:<COL_NUM>"

3. **Type Checker**
 * For type checking any syntactically correct file, type `checkType <path of input file>`
 * If there is a type mismatch or a use before declaration of a variable, then the type checker prints a suitable error message on the console.

4. **Evaluator**
 * The output of a valid input can be obtained by typing `evaluate <path of input file>`
 * Returns the value of different function applications in the input as a list of values.