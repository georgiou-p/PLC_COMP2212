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
Manual

The exact structure is up to you, but I would expect the following as a bare minimum:

    A description of the syntax of your language, with examples.
    Descriptions of any built-in commands or functions
    A rationale for your language - why did you design and implement it the way you have?
    A list of references that have influenced the design of your language

You should make sure to document all features of your language that go beyond what's strictly required by the given tasks. Examples of these extension features might include:

    A type system for your language
    Error handling
    File output
    Syntax highlighting for your language in your chosen IDE

(note that this is not an exhaustive list)

The word limit is 2500 words. I will not count code examples or references towards that limit.
Tasks
Task 6: Multiway Cartesian Product

Assume five inputs P:3, Q:3, R:1, S:1 and T:4. Your program should output the cartesian product of the five files so that each output row is created from the combination of rows from each of P, Q, R, S and T. The entries should appear in the output in the following order: for each row p1, p2, p3 in P, each row q1, q2, q3 in Q, each row r1 in R, each row s1 in S and each row t1, t2, t3, t4 in T, output a row p1, p2, p3, q1, q2, q3, r1, s1, t1, t2, t3, t4. The output arity is 12.
Example

P.csv:

    1,2,3
    4,5,6
    7,8,9

Q.csv:

    foo,bar,baz
    baz,qux,quux

R.csv:

    aardvark

S.csv:

    23

T.csv:

    a,b,c,d

Output:

    1,2,3,baz,qux,quux,aardvark,23,a,b,c,d
    1,2,3,foo,bar,baz,aardvark,23,a,b,c,d
    4,5,6,baz,qux,quux,aardvark,23,a,b,c,d
    4,5,6,foo,bar,baz,aardvark,23,a,b,c,d
    7,8,9,baz,qux,quux,aardvark,23,a,b,c,d
    7,8,9,foo,bar,baz,aardvark,23,a,b,c,d

Task 7: Paired Composition

Assume two input symbols F:3 and G:3 both of arity three. Your program should output the composition of the relations represented by F and G as follows: for each row f1, f2, f3 in F and each row g1, g2, g3 in G, output a row containing f1, g3 if and only if both of f2 and f3 are non-empty, f2=g1 and f3=g2. The output arity is 2.
Example

F.csv:

    a,b,c
    d,e,
    g,h,i
    j,k,l

G.csv:

    b,c,d
    b,c,e
    h,,i
    k,l,
    k,l,l

Output:

    a,d
    a,e
    j,
    j,l

Task 8: Right Merge on Last Column

Assume two inputs P:4 and Q:4. Your program should output rows as follows: for each row p1, p2, p3, p4 in P and each row q1, q2, q3, q4 in Q such that p4=q4, the output should contain a row r1, r2, r3, q4 where ri is pi if qi is empty and qi otherwise. The output arity is 4.
Example 1

P.csv:

    ,4,5,1
    ,2,,2
    2,1,7,3
    ,1,8,4

Q.csv:

    7,4,6,1
    3,5,8,2
    1,,,2
    3,2,,4

Output:

    1,2,,2
    3,2,8,4
    3,5,8,2
    7,4,6,1

Example 2

P.csv:

    Aaron,foo,,qux
    Brenda,,bar,quux

Q.csv:

    Ciara,bar,,quux
    Aaron,,baz,qux

Output:

    Aaron,foo,baz,qux
    Ciara,bar,bar,quux

Example 3

P.csv:

    1,6,2,3
    2,7,4,5

Q.csv:

    3,8,6,6
    4,9,5,3

Output:

    4,9,5,3

Task 9: Paths of length three

Assume a single input R:2. Your program should compute all paths of length 3 in the relation represented by R. A path of length 3 in R is a sequence r1, r2, r3, r4 such that r1, r2 is a row in R, r2, r3 is a row in R and r3, r4 is a row in R. For each such path of length 3, the output should contain a row r1, r4.

Example 1

R.csv:

    Romsey,Eastleigh
    Eastleigh,Southampton Airport Parkway
    Southampton Airport Parkway,Southampton Central
    Southampton Central,Romsey
    Romsey, Salisbury

Output:

    Eastleigh,Romsey
    Romsey,Southampton Central
    Southampton Airport Parkway,Eastleigh
    Southampton Airport Parkway,Salisbury
    Southampton Central,Southampton Airport Parkway

Example 2

R.csv:

    foo,foo

Outout:

    foo,foo

Task 10: Matching Pairs

Assume two inputs S:3 and T:3. Your program should compute the output as follows: for each row s1, s2, s3 in S and each row t1, t2, t3 in T , output s3, t1 if s1=s2 and t2=t3.
Example 1

S.csv:

    A,A,B
    A,B,B
    A,B,C

T.csv:

    A,A,B
    A,B,B
    A,B,C
    C,D,D

Output:

    B,A
    B,C

Example 2

S.csv:

    A,B,B
    B,C,D
    ,,

T.csv:

    B,B,B
    B,C,D
    A,,

Output:

    ,A
    ,B
