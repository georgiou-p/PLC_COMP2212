-- Task 5: Left merge on first column
-- For each pair of rows p1,p2,p3,p4 in P and q1,q2,q3,q4 in Q such that p1=q1,
-- Output p1,r2,r3,r4 where ri is qi if pi is empty and pi otherwise

SELECT P.1 FROM P:4 LEFT MERGE Q:4 ON P.2 = Q.2