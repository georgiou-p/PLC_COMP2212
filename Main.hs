-- Test.hs
module Main where

import Lexer
import Parser
import AST

main :: IO ()
main = do
  let query = "SELECT A.1, A.2 FROM A:2"
  putStrLn "Original query:"
  putStrLn query
  putStrLn "\nTokens:"
  let tokens = alexScanTokens query
  print tokens
  putStrLn "\nParsed AST:"
  let ast = parseQuery query
  print ast