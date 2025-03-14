{
module Lexer (
  Token(..),
  AlexPosn(..),
  alexScanTokens,
  tokenPosn
) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+                               ;
  "--".*                                ;  -- Comment until end of line
  
  SELECT                                { \p s -> TokenSelect p }
  FROM                                  { \p s -> TokenFrom p }
  WHERE                                 { \p s -> TokenWhere p }
  JOIN                                  { \p s -> TokenJoin p }
  ON                                    { \p s -> TokenOn p }
  COALESCE                              { \p s -> TokenCoalesce p }
  AND                                   { \p s -> TokenAnd p }
  OR                                    { \p s -> TokenOr p }
  
  ","                                   { \p s -> TokenComma p }
  "."                                   { \p s -> TokenDot p }
  ":"                                   { \p s -> TokenColon p }
  "="                                   { \p s -> TokenEquals p }
  "!="                                  { \p s -> TokenNotEquals p }
  "("                                   { \p s -> TokenLParen p }
  ")"                                   { \p s -> TokenRParen p }
  
  \"[^\"]*\"                            { \p s -> TokenString p (init (tail s)) }
  [$alpha][$alnum\_]*                   { \p s -> TokenIdentifier p s }
  $digit+                               { \p s -> TokenInteger p (read s) }

{
data Token = 
    TokenSelect AlexPosn
  | TokenFrom AlexPosn
  | TokenWhere AlexPosn
  | TokenJoin AlexPosn
  | TokenOn AlexPosn
  | TokenCoalesce AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenComma AlexPosn
  | TokenDot AlexPosn
  | TokenColon AlexPosn
  | TokenEquals AlexPosn
  | TokenNotEquals AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenString AlexPosn String
  | TokenIdentifier AlexPosn String
  | TokenInteger AlexPosn Int
  | TokenEOF AlexPosn
  deriving (Eq, Show)

tokenPosn :: Token -> AlexPosn
tokenPosn (TokenSelect p) = p
tokenPosn (TokenFrom p) = p
tokenPosn (TokenWhere p) = p
tokenPosn (TokenJoin p) = p
tokenPosn (TokenOn p) = p
tokenPosn (TokenCoalesce p) = p
tokenPosn (TokenAnd p) = p
tokenPosn (TokenOr p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenDot p) = p
tokenPosn (TokenColon p) = p
tokenPosn (TokenEquals p) = p
tokenPosn (TokenNotEquals p) = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p
tokenPosn (TokenString p _) = p
tokenPosn (TokenIdentifier p _) = p
tokenPosn (TokenInteger p _) = p
tokenPosn (TokenEOF p) = p
}
