{
module Parser (
  parseQuery
) where

import Lexer
import qualified AST
import Data.Maybe (fromMaybe)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  SELECT      { TokenSelect _ }
  FROM        { TokenFrom _ }
  WHERE       { TokenWhere _ }
  JOIN        { TokenJoin _ }
  ON          { TokenOn _ }
  COALESCE    { TokenCoalesce _ }
  AND         { TokenAnd _ }
  OR          { TokenOr _ }
  ','         { TokenComma _ }
  '.'         { TokenDot _ }
  ':'         { TokenColon _ }
  '='         { TokenEquals _ }
  '!='        { TokenNotEquals _ }
  '('         { TokenLParen _ }
  ')'         { TokenRParen _ }
  string      { TokenString _ $$ }
  identifier  { TokenIdentifier _ $$ }
  integer     { TokenInteger _ $$ }

%right OR
%right AND
%nonassoc '=' '!='

%%

Query : SELECT SelectList FROM SourceList OptWhere OptJoins  { AST.CQLQuery $2 $4 $5 $6 }

SelectList : SelectExpr                    { [$1] }
           | SelectExpr ',' SelectList     { $1 : $3 }

SelectExpr : identifier '.' integer        { AST.ColumnRef $1 $3 }
           | string                        { AST.Constant $1 }
           | COALESCE '(' SelectExpr ',' SelectExpr ')'  { AST.Coalesce $3 $5 }

SourceList : Source                      { [$1] }
           | Source ',' SourceList       { $1 : $3 }

Source : identifier ':' integer          { AST.SourceDecl $1 $3 }

OptWhere : WHERE Condition               { Just $2 }
         |                               { Nothing }

Condition : SelectExpr '=' SelectExpr    { AST.Equals $1 $3 }
          | SelectExpr '!=' SelectExpr   { AST.NotEquals $1 $3 }
          | Condition AND Condition      { AST.And $1 $3 }
          | Condition OR Condition       { AST.Or $1 $3 }
          | '(' Condition ')'            { $2 }

OptJoins : JoinList                      { $1 }
         |                               { [] }

JoinList : Join                          { [$1] }
         | Join JoinList                 { $1 : $2 }

Join : JOIN Source ON Condition          { AST.JoinClause $2 $4 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at " ++ 
                           (case tokens of
                              [] -> "end of file"
                              (t:_) -> show (tokenPosn t))

parseQuery :: String -> AST.CQLQuery
parseQuery = parse . alexScanTokens
}