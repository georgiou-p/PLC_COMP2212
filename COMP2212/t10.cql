-- Task 10: Matching Pairs
-- Output s3, t1 for each pair in S and T where s1=s2 and t2=t3
SELECT S.3, T.1
FROM S:3, T:3
WHERE S.1 = S.2
AND T.2 = T.3