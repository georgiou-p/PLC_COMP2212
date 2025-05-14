-- Task 9: Paths of length three
-- Find all paths of length 3 in relation R
SELECT X.1, Z.2
FROM 
  (SELECT R.1, R.2 FROM R:2) AS X,
  (SELECT R.1, R.2 FROM R:2) AS Y,
  (SELECT R.1, R.2 FROM R:2) AS Z
WHERE X.2 = Y.1 AND Y.2 = Z.1