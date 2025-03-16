module AST where

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