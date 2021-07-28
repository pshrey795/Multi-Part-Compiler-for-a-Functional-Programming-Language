Testcases
===

This directory is partitioned into two sub-directories:

Lexer/Parser:
---

[This folder](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/tree/master/Compiler/TestCases/Lexer&Parser) contains the testcases for the lexer and the parser. It contains 6 testcases in total, which can be valid or invalid. In case of a valid input, the lexer is supposed to give a stream of tokens as output, and the parser is expected to contruct a parse tree. For comparison, I have included the manually drawn [parse trees](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/tree/master/Compiler/TestCases/Lexer&Parser/ParseTrees) for reference. In case of invalid input, the lexer or the parser should return a suitable error message.


Evaluator:
---

[This folder](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/tree/master/Compiler/TestCases/Evaluator) contains the testcases for semantic verification i.e. to check if the input files contain semantically correct code. The type checker verifies the input file and if verified, the evaluator is expected to return the value of the result of the output. There are even a few specialized test cases for checking higher order functions, recursion, etc.