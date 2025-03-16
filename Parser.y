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
  AS          { TokenAs _ }
  AND         { TokenAnd _ }
  OR          { TokenOr _ }
  NOT         { TokenNot _ }
  IS          { TokenIs _ }
  NULL        { TokenNull _ }
  EMPTY       { TokenEmpty _ }
  PRODUCT     { TokenProduct _ }
  CONSTANT    { TokenConstantKw _ }
  LEFT        { TokenLeft _ }
  MERGE       { TokenMerge _ }
  DISTINCT    { TokenDistinct _ }
  CARTESIAN   { TokenCartesian _ }
  EXISTS      { TokenExists _ }
  PERMUTE     { TokenPermute _ }
  DROP        { TokenDrop _ }
  COPY        { TokenCopy _ }
  RENAME      { TokenRename _ }
  CREATE      { TokenCreate _ }
  PROJECT     { TokenProject _ }
  COLREF      { TokenColRef _ }
  GROUP       { TokenGroup _ }
  BY          { TokenBy _ }
  HAVING      { TokenHaving _ }
  ORDER       { TokenOrder _ }
  DESC        { TokenDesc _ }
  ASC         { TokenAsc _ }
  UNION       { TokenUnion _ }
  INTERSECT   { TokenIntersect _ }
  EXCEPT      { TokenExcept _ }
  ','         { TokenComma _ }
  '.'         { TokenDot _ }
  ':'         { TokenColon _ }
  '='         { TokenEquals _ }
  '!='        { TokenNotEquals _ }
  '('         { TokenLParen _ }
  ')'         { TokenRParen _ }
  '*'         { TokenStar _ }
  string      { TokenString _ $$ }
  identifier  { TokenIdentifier _ $$ }
  integer     { TokenInteger _ $$ }

%right OR
%right AND
%nonassoc '=' '!=' IS NOT
%left '*'

%%

Query : SELECT SelectList FROM SourceList OptWhere OptJoins
        { AST.CQLQuery $2 $4 $5 $6 }
      | SELECT DISTINCT SelectList FROM SourceList OptWhere OptJoins
        { AST.DistinctQuery $3 $5 $6 $7 }
      | SELECT SelectList FROM SourceList PRODUCT SourceList
        { AST.CartesianProduct $2 $4 $6 }
      | SELECT SelectList FROM SourceList CARTESIAN SourceList
        { AST.CartesianProduct $2 $4 $6 }
      | SELECT SelectList FROM SourceList LEFT MERGE SourceList ON Condition
        { AST.LeftMerge $2 $4 $7 $9 }
      | PERMUTE SourceList COLREF integer COLREF integer
        { AST.PermuteCols $2 $4 $6 }
      | DROP SourceList WHERE Condition
        { AST.DropRows $2 $4 }
      | COPY SourceList
        { AST.CopyTable $2 }
      | RENAME SourceList COLREF integer AS identifier
        { AST.RenameCol $2 $4 $6 }
      | PROJECT SelectList FROM SourceList
        { AST.ProjectCols $2 $4 }
      | SELECT SelectList FROM SourceList OptWhere GROUP BY GroupByList OptHaving
        { AST.GroupByQuery $2 $4 $5 $8 $9 }
      | Query UNION Query
        { AST.UnionQuery $1 $3 }
      | Query INTERSECT Query
        { AST.IntersectQuery $1 $3 }
      | Query EXCEPT Query
        { AST.ExceptQuery $1 $3 }

SelectList : '*'                         { [AST.AllColumns] }
           | SelectExpr                  { [$1] }
           | SelectExpr ',' SelectList   { $1 : $3 }

SelectExpr : identifier '.' integer      { AST.ColumnRef $1 $3 }
           | identifier                  { AST.TableRef $1 }
           | string                      { AST.Constant $1 }
           | CONSTANT '(' string ')'     { AST.Constant $3 }
           | COALESCE '(' SelectExpr ',' SelectExpr ')'  { AST.Coalesce $3 $5 }
           | SelectExpr AS identifier    { AST.AliasExpr $1 $3 }

SourceList : Source                      { [$1] }
           | Source ',' SourceList       { $1 : $3 }

Source : identifier ':' integer          { AST.SourceDecl $1 $3 }
       | identifier                      { AST.SourceTable $1 }

OptWhere : WHERE Condition               { Just $2 }
         |                               { Nothing }

Condition : SelectExpr '=' SelectExpr    { AST.Equals $1 $3 }
          | SelectExpr '!=' SelectExpr   { AST.NotEquals $1 $3 }
          | SelectExpr IS EMPTY          { AST.IsEmpty $1 }
          | SelectExpr IS NOT EMPTY      { AST.IsNotEmpty $1 }
          | SelectExpr IS NULL           { AST.IsNull $1 }
          | SelectExpr IS NOT NULL       { AST.IsNotNull $1 }
          | Condition AND Condition      { AST.And $1 $3 }
          | Condition OR Condition       { AST.Or $1 $3 }
          | NOT Condition                { AST.Not $2 }
          | '(' Condition ')'            { $2 }

OptJoins : JoinList                      { $1 }
         |                               { [] }

JoinList : Join                          { [$1] }
         | Join JoinList                 { $1 : $2 }

Join : JOIN Source ON Condition          { AST.JoinClause $2 $4 }

GroupByList : GroupByExpr                    { [$1] }
            | GroupByExpr ',' GroupByList    { $1 : $3 }

GroupByExpr : identifier '.' integer         { AST.ColumnRef $1 $3 }
            | identifier                     { AST.TableRef $1 }

OptHaving : HAVING Condition                 { Just $2 }
          |                                  { Nothing }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at " ++ 
                           (case tokens of
                              [] -> "end of file"
                              (t:_) -> show (tokenPosn t))

parseQuery :: String -> AST.CQLQuery
parseQuery = parse . alexScanTokens
}