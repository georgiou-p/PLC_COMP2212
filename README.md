# PLC_COMP2212

Introduction

In this coursework you are required to design and implement a domain specific programming language for querying CSV documents. There are many query languages for similar data formats, such as datalog for deductive databases or SQL for relational databases. You are welcome to research any existing query languages to inspire the design of your own language. If you want, you could stick closely to the syntax of an existing language, but you are also more than welcome to go your own way and be as original and creative as you want: it is YOUR programming language, and your design decisions.

You are required to (1) invent an appropriate syntax and (2) write an interpreter (possibly using alex and happy for lexing and parsing). Your overall goal is to be able to write a program for any particular query of csv input files.

The coursework has two submissions with due dates and weights as follows:

    4pm on Thu 8th May, worth 20% of the module mark
    4pm on Thu 15th May, worth 20% of the module mark

For Submission 1, you will be required to submit the Haskell source code for the interpreter for your language along with five programs written in your language that satisfy the first five tasks listed below.

For Submission 2, you will be required to submit a manual for your language along with five programmes written in your language that will satisfy a second group of five (currently unseen) tasks that will be released after the Submission 1 deadline. Please keep a record of all the sources you consult that influence your design, and include them in your programming language manual for Submission 2. You should anticipate that the additional problems will consist of variations and combinations of the first five problems.

The specification is deliberately loose. If we haven’t specified something, it is a design decision for you to make: e.g. how to handle syntax errors, illegal inputs, whether to allow comments, support for syntax highlighting, compile time warnings, any type systems etc.
Notes on Tasks

For each task, we will declare a number of inputs. For every input A, you should assume that we will place a CSV file called A.csv in the directory where we execute your interpreter. The file will always be compatible with the arity of A; if A is of arity 3 (written A:3), then every row of A.csv will contain three entries (i.e. two commas).

Although the tasks for Submission 1 are very specific in their requirements, you should bear in mind that Submission 2 will require you to apply the operations more generally; you should consider how best to generalise the operations in your language.

The entries in the CSV files are to be considered as strings and there may be empty entries in the input file. Leading and trailing whitespace in entries should not be included as part of the string value for that entry.

For example a file with arity 3 may contain entries such as

    snap,crackle,pop

and

    goodbye, , cruel world

In the latter example, the entry in the second column is an empty string and the value in the third column is the string "cruel world". For simplicity you may assume that no entries contain the comma symbol (or any escaped form of it).

The output should also be in CSV format. The number of columns and rows required will be determined by the problem. Leading and trailing whitespace should be removed from entries. The output should always be sorted lexicographically by columns from left to right with the default Haskell String ordering for string value entries. Note that the empty string should sort before any non-empty string.

The correctness of your solutions will be checked with a number of automated tests. We will release an “automarker” script before the first deadline which will give you an indication as to how your programs will perform in our test harness.
Submission 1

You are required to submit two files by 4pm on Thu 8th May, as follows:

    a zip file containing the source code for your language interpreter, written in Haskell
    a zip file containing five programs, written in your language, that solve the five tasks below. The programs should be in files named t1.cql, t2.cql, t3.cql, t4.cql and t5.cql.

Each task in Submission 1 will be worth 10% of the overall coursework mark (i.e. 4% of the module mark).
Task 1: Cartesian Product

Assume two inputs A:2 and B:2 (i.e. both of arity two). Your program should output the cartesian product of the two relations such that each row of the output is created from each pairing of a row from A and a row from B, with entries output in the following order: for each row a1,a2 in A and each row b1,b2 in B, output a row a1,a2,b1,b2. The output arity is therefore four.
Example 1

A.csv:

    Aardvark, Aaron

B.csv:

    Beagle , Brenda

Output:

    Aardvark,Aaron,Beagle,Brenda

Note that leading and trailing whitespace in the inputs has been removed in the output.
Example 2

A.csv:

    1, 2
    1, 2

B.csv:

    3, 4
    3, 4

Output:

    1,2,3,4
    1,2,3,4
    1,2,3,4
    1,2,3,4

Example 3

A.csv:

     

B.csv:

    foo,bar

Output:

Note that in this example the file A.csv is empty, so there are no possible input rows; for this reason, the output is empty.
Task 2: Permutation, Drop and Matching

Assume a single input A:3. For each input row a1,a2,a3 of A, your program should output a row a3,a1 if and only if a1 and a2 are equal. The output arity is two.
Example 1

A.csv:

    Brenda,Brenda,Beagle
    Aaron,Aaron,Aardvark
    Ciara,Caterpillar,Caterpillar

Output:

    Aardvark,Aaron
    Beagle,Brenda

Note that the output is sorted lexicographically: for this reason, the row starting with "Aardvark" comes before the row starting with "Beagle"
Example 2

A.csv

    1,3,6
    1,2,2
    2,2,2
    2,2,6

Output:

    2,2
    6,2

Example 3

A.csv:

    1,1,
    1,1,2
    5,4,3
    4,4,1
    ,,5

Output:

    ,1
    1,4
    2,1
    5,

Task 3: Existence Check

Assume a single input A:2. For each row a1,a2 in A, your program should output a row a1,a2 if and only if a2 is not the empty string. The output arity is 2.
Example 1

A.csv

    Ciara, Caterpillar
    Aaron, Aardvark
    Brenda ,Beagle

Output:

    Aaron,Aardvark
    Brenda,Beagle
    Ciara,Caterpillar

Example 2

A.csv:

    Eric ,Eagle
    Dorothy,
    Aaron,
    Ciara,Caterpillar
    Brenda,Beagle

Output:

    Brenda,Beagle
    Ciara,Caterpillar
    Eric,Eagle

Task 4: Copying and Constants

Assume a single input A:1. For each row a1 in A, your program should output a row a1,foo,a1 where "foo" is a fixed string.
Example 1

A.csv:

    Brenda
    Eric
    Ciara

Output:

    Brenda,foo,Brenda
    Ciara,foo,Ciara
    Eric,foo,Eric

Example 2

A.csv

    Eric

    Brenda

Output:

    ,foo,
    Brenda,foo,Brenda
    Eric,foo,Eric

Note that the empty line in A.csv is treated as an empty string value.
Task 5: Left merge on first column

Assume two inputs P:4 and Q:4. For each pair of rows p1,p2,p3,p4 in P and q1,q2,q3,q4 in Q such that p1=q1, your program should output a row p1,r2,r3,r4 where ri is qi if pi is empty and pi otherwise. The output arity is 4.
Example 1

P.csv:

    1,5,4,
    2,,2,
    3,7,1,2
    4,8,,

Q.csv:

    1,6,4,7
    2,8,5,3
    2,,,1
    4,,2,3

Output:

    1,5,4,7
    2,,2,1
    2,8,2,3
    4,8,2,3

Example 2

P.csv:

    Aaron,foo,,baz
    Brenda,,bar,baz

Q.csv:

    Brenda,foo,,
    Aaron,,bar,

Output:

    Aaron,foo,bar,baz
    Brenda,foo,bar,baz

Example 3

P.csv:

    1,6,2,3
    2,7,4,5

Q.csv:

    3,8,6,6
    4,9,5,3

Output:

          

Submission 2

You are required to submit two files by 4pm on Thu 15th May, as follows:

    a pdf file named manual.pdf containing the manual for your language
    a zip file containing five programs, written in your language, that solve the five tasks below. The programs should be in files named t6.cql, t7.cql, t8.cql, t9.cql and t10.cql.

Each task in Submission 2 will be worth 5% of the overall coursework mark (i.e. 2% of the module mark). The language manual will be worth 25% of the overall coursework mark (i.e. 10% of the module mark).
Tasks TBA

The tasks in Submission 2 will be generalisations or combinations of tasks in Submission 1, or will be reasonable extensions from those tasks.
