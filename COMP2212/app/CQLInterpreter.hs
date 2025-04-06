-- CQLInterpreter.hs
module Main where

import System.Environment
import System.IO
import Data.List
import CQLParser
import CQLEvaluator
import qualified Data.Map as Map


-- Main function to run the interpreter
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-i"] -> do
      putStrLn "Interactive CQL Interpreter"
      putStrLn "Type 'help' for instructions, 'quit' to exit"
      interactiveMode
    ["-q", queryString] -> do
      result <- evalMain queryString
      putStrLn "Query result:"
      putStrLn $ tableToString result
    [filename] -> do
      runFile filename
    _ -> do
      putStrLn "Usage: cql-interpreter [filename] [-q query] [-i]"
      putStrLn "  filename  ->  Execute the query in the specified file"
      putStrLn "  -q query  ->  Execute the specified query"
      putStrLn "  -i        ->  Start in interactive mode"

-- Interactive mode
interactiveMode :: IO ()
interactiveMode = do
  putStr "CQL> "
  hFlush stdout
  input <- getLine
  case input of
    "quit" -> putStrLn "Goodbye!"
    "exit" -> putStrLn "Goodbye!"
    "help" -> do
      putStrLn "CQL Interpreter Help:"
      putStrLn "  Enter a CQL query to execute it"
      putStrLn "  Type 'quit' or 'exit' to exit"
      putStrLn "  Type 'load filename' to load and execute a query from a file"
      putStrLn "  Type 'help' to display this help message"
      interactiveMode
    _ -> if "load " `isPrefixOf` input
         then do
           let filename = drop 5 input
           runFile filename
           interactiveMode
         else do
           result <- evalMain input
           putStrLn "Query result:"
           putStrLn $ tableToString result
           interactiveMode

-- Run a query from a file
runFile :: String -> IO ()
runFile filename = do
  putStrLn $ "Executing query from file: " ++ filename
  contents <- readFile filename
  result <- evalMain contents
  putStrLn "Query result:"
  putStrLn $ tableToString result

-- Convert a table to a string representation
tableToString :: Table -> String
tableToString [] = "Empty result"
tableToString table = unlines $ map formatRow table
  where
    formatRow :: Row -> String
    formatRow = intercalate "," . map preserveEmpty
    
    preserveEmpty :: String -> String
    preserveEmpty "" = ""
    preserveEmpty s = s

-- Implementation of the task examples

-- Task 1: Cartesian Product
task1 :: IO ()
task1 = do
  let query = "SELECT * FROM A:2 PRODUCT B:2"
  result <- evalMain query
  writeFile "task1_result.csv" $ unlines $ map (intercalate ",") result
  putStrLn "Task 1 completed. Results written to task1_result.csv"

-- Task 2: Permutation, Drop and Matching
task2 :: IO ()
task2 = do
  let query = "SELECT A.3, A.1 FROM A:3 WHERE A.1 = A.2"
  result <- evalMain query
  writeFile "task2_result.csv" $ unlines $ map (intercalate ",") result
  putStrLn "Task 2 completed. Results written to task2_result.csv"

-- Task 3: Existence Check
task3 :: IO ()
task3 = do
  let query = "SELECT A.1, A.2 FROM A:2 WHERE A.2 IS NOT EMPTY"
  result <- evalMain query
  writeFile "task3_result.csv" $ unlines $ map (intercalate ",") result
  putStrLn "Task 3 completed. Results written to task3_result.csv"

-- Task 4: Copying and Constants
task4 :: IO ()
task4 = do
  let query = "SELECT A.1, CONSTANT(\"foo\"), A.1 FROM A:1"
  result <- evalMain query
  writeFile "task4_result.csv" $ unlines $ map (intercalate ",") result
  putStrLn "Task 4 completed. Results written to task4_result.csv"

-- Task 5: Left merge on first column
task5 :: IO ()
task5 = do
  let query = "SELECT P.1, COALESCE(P.2, Q.2), COALESCE(P.3, Q.3), COALESCE(P.4, Q.4) FROM P:4 LEFT MERGE Q:4 ON P.1 = Q.1"
  result <- evalMain query
  writeFile "task5_result.csv" $ unlines $ map (intercalate ",") result
  putStrLn "Task 5 completed. Results written to task5_result.csv"

-- Run all tasks
runAllTasks :: IO ()
runAllTasks = do
  task1
  task2
  task3
  task4
  task5
  putStrLn "All tasks completed."