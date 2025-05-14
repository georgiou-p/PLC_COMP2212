-- Task 7: Paired Composition
-- Output f1, g3 for each row in F and G where f2=g1, f3=g2, and f2 and f3 are non-empty
SELECT F.1, G.3 
FROM F:3, G:3 
WHERE F.2 = G.1 
AND F.3 = G.2 
AND F.2 IS NOT EMPTY 
AND F.3 IS NOT EMPTY