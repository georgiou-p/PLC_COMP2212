-- Task 8: Right Merge on Last Column
-- For matching rows on last column, output coalesced values
SELECT 
    COALESCE(Q.1, P.1), 
    COALESCE(Q.2, P.2), 
    COALESCE(Q.3, P.3), 
    Q.4
FROM P:4, Q:4 
WHERE P.4 = Q.4