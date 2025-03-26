-- Task 4: Copying and Constants
-- For each row a1 in A, output a1,foo,a1 where "foo" is a fixed string

SELECT A.1, CONSTANT("foo"), A.1 FROM A:1