module CQLEvaluator (
  evaluateQuery,
  Row,
  Table,
  evalMain
) where

import CQLParser
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Control.Monad

type Value = String
type Row = [Value]
type Table = [Row]
type TableEnv = Map.Map String Table

-- Read a CSV file and parse it
readCSV :: FilePath -> IO Table
readCSV path = do
  content <- readFile path
  return $ map parseLine $ lines content
  where
    parseLine :: String -> Row
    parseLine line = splitOn ',' line

    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]  -- Handle empty line case
    splitOn delim s = 
        let (token, rest) = break (== delim) s
        in token : if null rest 
                  then [] 
                  else splitOn delim (tail rest)

    trim :: String -> String
    trim "" = ""  -- Keep empty strings as is
    trim s = reverse $ dropWhile (== ' ') $ reverse $ dropWhile (== ' ') s

-- Write a table to a CSV file
writeCSV :: FilePath -> Table -> IO ()
writeCSV path table = writeFile path $ unlines $ map (intercalate ",") table

-- Load source tables according to query requirements
loadSources :: [Source] -> IO TableEnv
loadSources sources = do
  tables <- forM sources $ \source -> case source of
    SourceTable name -> do
      contents <- readCSV (name ++ ".csv")
      return (name, contents)
    SourceDecl name arity -> do
      contents <- readCSV (name ++ ".csv")
      -- Validate that each row has the specified arity
      let validContents = filter (\row -> length row == arity) contents
      return (name, validContents)
  return $ Map.fromList tables

-- Evaluate an expression with respect to the current row and environment
evalExpr :: TableEnv -> Map.Map String Int -> Row -> Expr -> Value
evalExpr env indices currentRow expr = case expr of
  ColumnRef tableName colIndex -> 
    let tableRows = fromMaybe [] $ Map.lookup tableName env
        tableIdx = fromMaybe (-1) $ Map.lookup tableName indices
        startIdx = tableIdx * (length (head $ fromMaybe [[]] $ Map.lookup tableName env))
        actualIdx = startIdx + colIndex - 1
    in if actualIdx >= 0 && actualIdx < length currentRow
       then currentRow !! actualIdx
       else ""
  
  TableRef tableName ->
    -- For simplicity, returns the tableName itself
    tableName
  
  Constant str -> str
  
  Coalesce expr1 expr2 ->
    let val1 = evalExpr env indices currentRow expr1
    in if val1 /= "" then val1 else evalExpr env indices currentRow expr2
  
  AliasExpr expr _ -> evalExpr env indices currentRow expr
  
  AllColumns -> error "Cannot evaluate '*' directly in this context"

-- Evaluate a condition with respect to the current row and environment
evalCondition :: TableEnv -> Map.Map String Int -> Row -> Condition -> Bool
evalCondition env indices currentRow cond = case cond of
  Equals expr1 expr2 ->
    evalExpr env indices currentRow expr1 == evalExpr env indices currentRow expr2
  
  NotEquals expr1 expr2 ->
    evalExpr env indices currentRow expr1 /= evalExpr env indices currentRow expr2
  
  IsEmpty expr ->
    evalExpr env indices currentRow expr == ""
  
  IsNotEmpty expr ->
    evalExpr env indices currentRow expr /= ""
  
  IsNull expr ->
    evalExpr env indices currentRow expr == ""
  
  IsNotNull expr ->
    evalExpr env indices currentRow expr /= ""
  
  And cond1 cond2 ->
    evalCondition env indices currentRow cond1 && evalCondition env indices currentRow cond2
  
  Or cond1 cond2 ->
    evalCondition env indices currentRow cond1 || evalCondition env indices currentRow cond2
  
  Not cond1 ->
    not $ evalCondition env indices currentRow cond1

-- Generate all possible combinations of rows for cartesian product
cartesianProduct :: [Table] -> Table
cartesianProduct [] = [[]]
cartesianProduct (t:ts) = [r1 ++ r2 | r1 <- t, r2 <- cartesianProduct ts]

-- Extract all columns based on select expressions
extractColumns :: TableEnv -> Map.Map String Int -> Row -> [Expr] -> Row
extractColumns env indices currentRow exprs = 
  map (evalExpr env indices currentRow) exprs

-- Main evaluation function for CQL queries
evaluateQuery :: CQLQuery -> IO Table
evaluateQuery query = case query of
  CQLQuery selectExprs sources maybeWhere joins -> do
    env <- loadSources sources
    let tableIndices = Map.fromList $ zip (map sourceName sources) [0..]
        
        -- Perform joins if any
        joinedTable = if null joins
                      then cartesianProduct (map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources)
                      else performJoins env tableIndices sources joins
        
        -- Apply WHERE clause if present
        filteredTable = case maybeWhere of
          Nothing -> joinedTable
          Just whereCondition -> filter (\row -> evalCondition env tableIndices row whereCondition) joinedTable
        
        -- Extract selected columns
        resultTable = map (\row -> extractColumns env tableIndices row selectExprs) filteredTable
    
    -- Sort the result lexicographically
    return $ sortBy lexicographicSort resultTable
  
  DistinctQuery selectExprs sources maybeWhere joins -> do
    -- Similar to CQLQuery but with distinct rows
    result <- evaluateQuery (CQLQuery selectExprs sources maybeWhere joins)
    return $ nub result
  
  CartesianProduct selectExprs sources1 sources2 -> do
    env1 <- loadSources sources1
    env2 <- loadSources sources2
    let env = Map.union env1 env2
        tables1 = map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources1
        tables2 = map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources2
        prod1 = cartesianProduct tables1
        prod2 = cartesianProduct tables2
        result = [r1 ++ r2 | r1 <- prod1, r2 <- prod2]
        
        tableIndices = Map.fromList $ zip (map sourceName (sources1 ++ sources2)) [0..]
        resultTable = map (\row -> extractColumns env tableIndices row selectExprs) result
    
    return $ sortBy lexicographicSort resultTable
  
  LeftMerge selectExprs sources1 sources2 condition -> do
      env1 <- loadSources sources1
      env2 <- loadSources sources2
      let env = Map.union env1 env2
          table1 = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources1
          table2 = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources2

          -- Build table indices mapping
          tableIndices = Map.fromList $ zip (map sourceName (sources1 ++ sources2)) [0..]
          
          -- For each row in table1, find matching rows in table2 according to condition
          mergedRows = concatMap (\row1 -> 
              let matches = filter (\row2 -> 
                      let combinedRow = row1 ++ row2
                          -- Update indices to account for table1 and table2
                          updatedIndices = Map.insert (sourceName (head sources2)) (length sources1) tableIndices
                      in evalCondition env updatedIndices combinedRow condition
                    ) table2
              in if null matches
                then []  -- No match: exclude this table1 row (different from standard LEFT JOIN)
                else map (\row2 -> row1 ++ row2) matches  -- Match: include table1 row combined with table2 rows
            ) table1
          
          -- Extract selected columns
          resultTable = map (\row -> extractColumns env tableIndices row selectExprs) mergedRows
      
      return $ sortBy lexicographicSort resultTable
  
  PermuteCols sources col1 col2 -> do
    env <- loadSources sources
    let tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
        permutedRows = map (\row -> 
            if length row > max col1 col2
            then [row !! (col1 - 1), row !! (col2 - 1)]
            else []
          ) tableRows
    
    return $ sortBy lexicographicSort $ filter (not . null) permutedRows
  
  DropRows sources condition -> do
    env <- loadSources sources
    let tableIndices = Map.fromList $ zip (map sourceName sources) [0..]
        tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
        filteredRows = filter (\row -> not $ evalCondition env tableIndices row condition) tableRows
    
    return $ sortBy lexicographicSort filteredRows
  
  CopyTable sources -> do
    env <- loadSources sources
    let tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
    
    return $ sortBy lexicographicSort tableRows
  
  RenameCol sources colIndex newName -> do
    env <- loadSources sources
    let tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
        -- In this simplified version, we're not actually renaming column headers
        -- as we're working with raw data only
    
    return $ sortBy lexicographicSort tableRows
  
  ProjectCols selectExprs sources -> do
    env <- loadSources sources
    let tableIndices = Map.fromList $ zip (map sourceName sources) [0..]
        tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
        projectedRows = map (\row -> extractColumns env tableIndices row selectExprs) tableRows
    
    return $ sortBy lexicographicSort projectedRows
  
  GroupByQuery selectExprs sources maybeWhere groupByExprs maybeHaving -> do
    -- This is a simplified implementation of GROUP BY
    -- A full implementation would require more complex aggregation functions
    env <- loadSources sources
    let tableIndices = Map.fromList $ zip (map sourceName sources) [0..]
        tableRows = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
        
        -- Filter rows based on WHERE clause
        filteredRows = case maybeWhere of
          Nothing -> tableRows
          Just whereCondition -> filter (\row -> evalCondition env tableIndices row whereCondition) tableRows
        
        -- Group rows by the result of groupByExprs
        groupKeyFn row = map (evalExpr env tableIndices row) groupByExprs
        grouped = groupBy (\r1 r2 -> groupKeyFn r1 == groupKeyFn r2) $ sortBy (\r1 r2 -> compare (groupKeyFn r1) (groupKeyFn r2)) filteredRows
        
        -- Take first row from each group (simplified aggregation)
        representativeRows = map head grouped
        
        -- Apply HAVING clause if present
        havingFiltered = case maybeHaving of
          Nothing -> representativeRows
          Just havingCondition -> filter (\row -> evalCondition env tableIndices row havingCondition) representativeRows
        
        -- Extract selected columns
        resultTable = map (\row -> extractColumns env tableIndices row selectExprs) havingFiltered
    
    return $ sortBy lexicographicSort resultTable
  
  UnionQuery query1 query2 -> do
    result1 <- evaluateQuery query1
    result2 <- evaluateQuery query2
    -- Union operation: combine rows and remove duplicates
    return $ sortBy lexicographicSort $ nub (result1 ++ result2)
  
  IntersectQuery query1 query2 -> do
    result1 <- evaluateQuery query1
    result2 <- evaluateQuery query2
    -- Intersection operation: keep only rows that appear in both results
    return $ sortBy lexicographicSort $ intersect result1 result2
  
  ExceptQuery query1 query2 -> do
    result1 <- evaluateQuery query1
    result2 <- evaluateQuery query2
    -- Except operation: keep only rows from result1 that don't appear in result2
    return $ sortBy lexicographicSort $ result1 \\ result2

-- Helper functions

-- Extract the name from a Source
sourceName :: Source -> String
sourceName (SourceDecl name _) = name
sourceName (SourceTable name) = name

-- Merge two rows for LEFT MERGE operation, replacing empty values in row1 with values from row2
mergeRows :: Row -> Row -> Row
mergeRows row1 row2 = zipWith mergeValue row1 row2
  where
    mergeValue v1 v2 
      | v1 == "" && v2 == "" = ""
      | v1 == "" = v2
      | otherwise = v1

-- Sort rows lexicographically
lexicographicSort :: Row -> Row -> Ordering
lexicographicSort = compare

-- Perform JOIN operations
performJoins :: TableEnv -> Map.Map String Int -> [Source] -> [JoinClause] -> Table
performJoins env tableIndices sources joins = 
  foldl' applyJoin baseTable joins
  where
    baseTable = cartesianProduct $ map (\s -> fromMaybe [] $ Map.lookup (sourceName s) env) sources
    
    applyJoin :: Table -> JoinClause -> Table
    applyJoin table (JoinClause joinSource joinCondition) = do
      let joinTableName = sourceName joinSource
          joinTable = fromMaybe [] $ Map.lookup joinTableName env
          
          -- For each row in the current result, combine with rows from joinTable
          -- that satisfy the join condition
          result = concatMap (\baseRow -> 
              let matchingRows = filter (\joinRow -> 
                      let combinedRow = baseRow ++ joinRow
                          -- Update tableIndices to include the joined table
                          updatedIndices = Map.insert joinTableName (length sources) tableIndices
                      in evalCondition env updatedIndices combinedRow joinCondition
                    ) joinTable
              in if null matchingRows
                 then [] -- Inner join: drop rows with no match
                 else map (\jr -> baseRow ++ jr) matchingRows
            ) table
      
      result

-- Main evaluation function that parses a query string and returns the result
evalMain :: String -> IO Table
evalMain queryStr = do
  let ast = parseQuery queryStr
  evaluateQuery ast