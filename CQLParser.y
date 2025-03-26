{
module CQLParser (
  parseQuery,
  CQLQuery(..),
  Source(..),
  Expr(..),
  Condition(..),
  JoinClause(..)
) where

import Lexer
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
        { CQLQuery $2 $4 $5 $6 }
      | SELECT DISTINCT SelectList FROM SourceList OptWhere OptJoins
        { DistinctQuery $3 $5 $6 $7 }
      | SELECT SelectList FROM SourceList PRODUCT SourceList
        { CartesianProduct $2 $4 $6 }
      | SELECT SelectList FROM SourceList CARTESIAN SourceList
        { CartesianProduct $2 $4 $6 }
      | SELECT SelectList FROM SourceList LEFT MERGE SourceList ON Condition
        { LeftMerge $2 $4 $7 $9 }
      | PERMUTE SourceList COLREF integer COLREF integer
        { PermuteCols $2 $4 $6 }
      | DROP SourceList WHERE Condition
        { DropRows $2 $4 }
      | COPY SourceList
        { CopyTable $2 }
      | RENAME SourceList COLREF integer AS identifier
        { RenameCol $2 $4 $6 }
      | PROJECT SelectList FROM SourceList
        { ProjectCols $2 $4 }
      | SELECT SelectList FROM SourceList OptWhere GROUP BY GroupByList OptHaving
        { GroupByQuery $2 $4 $5 $8 $9 }
      | Query UNION Query
        { UnionQuery $1 $3 }
      | Query INTERSECT Query
        { IntersectQuery $1 $3 }
      | Query EXCEPT Query
        { ExceptQuery $1 $3 }

SelectList : '*'                         { [AllColumns] }
           | SelectExpr                  { [$1] }
           | SelectExpr ',' SelectList   { $1 : $3 }

SelectExpr : identifier '.' integer      { ColumnRef $1 $3 }
           | identifier                  { TableRef $1 }
           | string                      { Constant $1 }
           | CONSTANT '(' string ')'     { Constant $3 }
           | COALESCE '(' SelectExpr ',' SelectExpr ')'  { Coalesce $3 $5 }
           | SelectExpr AS identifier    { AliasExpr $1 $3 }

SourceList : Source                      { [$1] }
           | Source ',' SourceList       { $1 : $3 }

Source : identifier ':' integer          { SourceDecl $1 $3 }
       | identifier                      { SourceTable $1 }

OptWhere : WHERE Condition               { Just $2 }
         |                               { Nothing }

Condition : SelectExpr '=' SelectExpr    { Equals $1 $3 }
          | SelectExpr '!=' SelectExpr   { NotEquals $1 $3 }
          | SelectExpr IS EMPTY          { IsEmpty $1 }
          | SelectExpr IS NOT EMPTY      { IsNotEmpty $1 }
          | SelectExpr IS NULL           { IsNull $1 }
          | SelectExpr IS NOT NULL       { IsNotNull $1 }
          | Condition AND Condition      { And $1 $3 }
          | Condition OR Condition       { Or $1 $3 }
          | NOT Condition                { Not $2 }
          | '(' Condition ')'            { $2 }

OptJoins : JoinList                      { $1 }
         |                               { [] }

JoinList : Join                          { [$1] }
         | Join JoinList                 { $1 : $2 }

Join : JOIN Source ON Condition          { JoinClause $2 $4 }

GroupByList : GroupByExpr                    { [$1] }
            | GroupByExpr ',' GroupByList    { $1 : $3 }

GroupByExpr : identifier '.' integer         { ColumnRef $1 $3 }
            | identifier                     { TableRef $1 }

OptHaving : HAVING Condition                 { Just $2 }
          |                                  { Nothing }

{
-- Main query data structure
data CQLQuery
  = CQLQuery [Expr] [Source] (Maybe Condition) [JoinClause]  -- Standard SELECT query
  | DistinctQuery [Expr] [Source] (Maybe Condition) [JoinClause]  -- SELECT DISTINCT
  | CartesianProduct [Expr] [Source] [Source]                -- Task 1: Cartesian product
  | LeftMerge [Expr] [Source] [Source] Condition             -- Task 5: Left merge
  | PermuteCols [Source] Int Int                            -- Permute specific columns
  | DropRows [Source] Condition                             -- Drop rows based on condition
  | CopyTable [Source]                                      -- Copy table
  | RenameCol [Source] Int String                           -- Rename column
  | ProjectCols [Expr] [Source]                             -- Project specific columns
  | GroupByQuery [Expr] [Source] (Maybe Condition) [Expr] (Maybe Condition) -- GROUP BY query
  | UnionQuery CQLQuery CQLQuery                            -- UNION of two queries
  | IntersectQuery CQLQuery CQLQuery                        -- INTERSECT of two queries
  | ExceptQuery CQLQuery CQLQuery                           -- EXCEPT of two queries
  deriving (Show, Eq)

-- Source declarations
data Source
  = SourceDecl String Int  -- Source with explicit arity, e.g., "A:2"
  | SourceTable String     -- Just a table name
  deriving (Show, Eq)

-- Expression types
data Expr
  = ColumnRef String Int   -- Table.Column reference, e.g., "A.1"
  | TableRef String        -- Just a table reference
  | Constant String        -- String constant, e.g., "foo"
  | Coalesce Expr Expr     -- COALESCE(expr1, expr2)
  | AliasExpr Expr String  -- Expression with alias, e.g., "A.1 AS col1"
  | AllColumns             -- "*" wildcard for selecting all columns
  deriving (Show, Eq)

-- Condition types for WHERE clauses and JOINs
data Condition
  = Equals Expr Expr       -- expr1 = expr2
  | NotEquals Expr Expr    -- expr1 != expr2
  | IsEmpty Expr           -- expr IS EMPTY
  | IsNotEmpty Expr        -- expr IS NOT EMPTY
  | IsNull Expr            -- expr IS NULL
  | IsNotNull Expr         -- expr IS NOT NULL
  | And Condition Condition -- cond1 AND cond2
  | Or Condition Condition  -- cond1 OR cond2
  | Not Condition           -- NOT cond
  deriving (Show, Eq)

-- Join clause
data JoinClause = JoinClause Source Condition
  deriving (Show, Eq)

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at " ++ 
                           (case tokens of
                              [] -> "end of file"
                              (t:_) -> show (tokenPosn t))

parseQuery :: String -> CQLQuery
parseQuery = parse . alexScanTokens
}
