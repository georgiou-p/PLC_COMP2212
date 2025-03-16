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
  AS                                    { \p s -> TokenAs p }
  AND                                   { \p s -> TokenAnd p }
  OR                                    { \p s -> TokenOr p }
  NOT                                   { \p s -> TokenNot p }
  IS                                    { \p s -> TokenIs p }
  NULL                                  { \p s -> TokenNull p }
  EMPTY                                 { \p s -> TokenEmpty p }
  PRODUCT                               { \p s -> TokenProduct p }
  CONSTANT                              { \p s -> TokenConstantKw p }
  LEFT                                  { \p s -> TokenLeft p }
  MERGE                                 { \p s -> TokenMerge p }
  DISTINCT                              { \p s -> TokenDistinct p }
  CARTESIAN                             { \p s -> TokenCartesian p }
  EXISTS                                { \p s -> TokenExists p }
  PERMUTE                               { \p s -> TokenPermute p }
  DROP                                  { \p s -> TokenDrop p }
  COPY                                  { \p s -> TokenCopy p }
  RENAME                                { \p s -> TokenRename p }
  CREATE                                { \p s -> TokenCreate p }
  PROJECT                               { \p s -> TokenProject p }
  COLREF                                { \p s -> TokenColRef p }
  GROUP                                 { \p s -> TokenGroup p }
  BY                                    { \p s -> TokenBy p }
  HAVING                                { \p s -> TokenHaving p }
  ORDER                                 { \p s -> TokenOrder p }
  DESC                                  { \p s -> TokenDesc p }
  ASC                                   { \p s -> TokenAsc p }
  UNION                                 { \p s -> TokenUnion p }
  INTERSECT                             { \p s -> TokenIntersect p }
  EXCEPT                                { \p s -> TokenExcept p }
  
  ","                                   { \p s -> TokenComma p }
  "."                                   { \p s -> TokenDot p }
  ":"                                   { \p s -> TokenColon p }
  "="                                   { \p s -> TokenEquals p }
  "!="                                  { \p s -> TokenNotEquals p }
  "("                                   { \p s -> TokenLParen p }
  ")"                                   { \p s -> TokenRParen p }
  "*"                                   { \p s -> TokenStar p }
  
  \"[^\"]*\"                            { \p s -> TokenString p (init (tail s)) }
  \'[^\']*\'                            { \p s -> TokenString p (init (tail s)) }
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
  | TokenAs AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenNot AlexPosn
  | TokenIs AlexPosn
  | TokenNull AlexPosn
  | TokenEmpty AlexPosn
  | TokenProduct AlexPosn
  | TokenConstantKw AlexPosn
  | TokenLeft AlexPosn
  | TokenMerge AlexPosn
  | TokenDistinct AlexPosn
  | TokenCartesian AlexPosn
  | TokenExists AlexPosn
  | TokenPermute AlexPosn
  | TokenDrop AlexPosn
  | TokenCopy AlexPosn
  | TokenRename AlexPosn
  | TokenCreate AlexPosn
  | TokenProject AlexPosn
  | TokenColRef AlexPosn
  | TokenGroup AlexPosn
  | TokenBy AlexPosn
  | TokenHaving AlexPosn
  | TokenOrder AlexPosn
  | TokenDesc AlexPosn
  | TokenAsc AlexPosn
  | TokenUnion AlexPosn
  | TokenIntersect AlexPosn
  | TokenExcept AlexPosn
  | TokenComma AlexPosn
  | TokenDot AlexPosn
  | TokenColon AlexPosn
  | TokenEquals AlexPosn
  | TokenNotEquals AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenStar AlexPosn
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
tokenPosn (TokenAs p) = p
tokenPosn (TokenAnd p) = p
tokenPosn (TokenOr p) = p
tokenPosn (TokenNot p) = p
tokenPosn (TokenIs p) = p
tokenPosn (TokenNull p) = p
tokenPosn (TokenEmpty p) = p
tokenPosn (TokenProduct p) = p
tokenPosn (TokenConstantKw p) = p
tokenPosn (TokenLeft p) = p
tokenPosn (TokenMerge p) = p
tokenPosn (TokenDistinct p) = p
tokenPosn (TokenCartesian p) = p
tokenPosn (TokenExists p) = p
tokenPosn (TokenPermute p) = p
tokenPosn (TokenDrop p) = p
tokenPosn (TokenCopy p) = p
tokenPosn (TokenRename p) = p
tokenPosn (TokenCreate p) = p
tokenPosn (TokenProject p) = p
tokenPosn (TokenColRef p) = p
tokenPosn (TokenGroup p) = p
tokenPosn (TokenBy p) = p
tokenPosn (TokenHaving p) = p
tokenPosn (TokenOrder p) = p
tokenPosn (TokenDesc p) = p
tokenPosn (TokenAsc p) = p
tokenPosn (TokenUnion p) = p
tokenPosn (TokenIntersect p) = p
tokenPosn (TokenExcept p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenDot p) = p
tokenPosn (TokenColon p) = p
tokenPosn (TokenEquals p) = p
tokenPosn (TokenNotEquals p) = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p
tokenPosn (TokenStar p) = p
tokenPosn (TokenString p _) = p
tokenPosn (TokenIdentifier p _) = p
tokenPosn (TokenInteger p _) = p
tokenPosn (TokenEOF p) = p
}
