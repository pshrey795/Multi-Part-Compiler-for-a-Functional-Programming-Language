###Testcases
===

####Correct Test Cases
---

* There are four files with correct format of a delimited file. They also contain all the intricacies like double quotes, newlines, delimiters and empty fields. The difference in the files is the delimiter used. All these files basically facilitate checking of the conversions between any two delimiters.

 1. himym.csv (comma(,))
 2. himym.asv (ampersand(&))
 3. himym.ssv (semicolon(;))
 4. himym.vsv (vertical bar(|))

* The test case "gene_alias.csv" is a very huge CSV file which checks if the program exceeds the desired execution time limit. 


####Incorrect Input Case
---

The testcase "himym_uneven.csv" is exactly same as "himym.csv" but one comma has been removed in Record 5, which results in one less field. The UnevenFieldException with string "Expected: 5 fields, Present: 4 fields on Line 5" will be thrown here.



In all the cases, the file output.csv will receive the output of any delimiter conversion, which can later be compared with other test cases provided, for correctness.
