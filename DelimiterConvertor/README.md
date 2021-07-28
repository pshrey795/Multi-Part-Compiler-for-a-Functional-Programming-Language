###Stage 1 : Delimiter Conversion
===

SML code for scanning a delimited code and converting it from given delimiter (say, a comma) to another delimiter(like a tab). Exact specification are contained in the [problem statement](https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language/tree/master/DelimiterConvertor/Statement.pdf).

####How to run the code:
---

1. First clone the directory using `git clone  https://github.com/pshrey795/Multi-Part-Compiler-for-a-Functional-Programming-Language`
2. The code is contained in the file _csv2dsv.sml_, navigate to the directory containing it.
3. Open the SML interactive environment using the command `sml` in terminal.
4. To compile the code, type `use "csv2dsv.sml"`.
5. The function "convertDelimiters" can be used for conversion, which takes 4 arguments:
 * Input File Path
 * Input Delimiter
 * Output File Path
 * Output Delimiter
 
**Note:** Some functions have already been pre-defined for common conversions like comma to tab and vice versa, in _"csv2tsv"_ and _"tsv2csv"_ respectively.
