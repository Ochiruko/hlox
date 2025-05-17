-- |
-- expression -> literal
--             | unary
--             | binary
--             | grouping ;
--
-- literal -> NUMBER | STRING | "true" | "false" | "nil" ;
--
-- grouping -> "(" expression ")" ;
--
-- unary -> ( "-" | "!" ) expression ;
--
-- binary -> expression operator expression ;
--
-- operator -> "==" | "!=" | "<" | ">" | ">="
--           | "+" | "-" | "*" | "/" ;

module Ast (Expr (..), BinaryOp (..), prettyPrint) where

-- | Type checking is delegated to evaluation.
-- Note that every operator ~ should be parsed as left-associative, (x ~ y) ~ z.
data BinaryOp = Comma
              | Equal
              | Unequal
              | LessThan
              | LessThanEquals
              | GreaterThan
              | GreaterThanEquals
              | Plus
              | Minus
              | Times
              | Over
              | Raise
              deriving (Show, Eq)

type Line = Int
type Offset = Int

-- | Inclusive in both the initialChar and finalChar
data Lexeme = Segment
  { initial :: (Line, Offset)
  , final :: (Line, Offset)
  } deriving (Show, Eq)

data Expr = Decorated
  { lexemes :: [Lexeme] -- ^ Only primitive lexemes should belong to this list.
  , expr :: UndecoratedExpr
  } deriving (Show, Eq)

data UndecoratedExpr where
  Number :: Double -> UndecoratedExpr
  String :: String -> UndecoratedExpr
  Identifier :: String -> UndecoratedExpr
  Ternary :: Expr -> Expr -> Expr -> UndecoratedExpr
  Boolean :: Bool -> UndecoratedExpr
  Not :: Expr -> UndecoratedExpr
  Negative :: Expr -> UndecoratedExpr
  Binary :: BinaryOp -> Expr -> Expr -> UndecoratedExpr
  FunctionCall :: String -> [Expr] -> UndecoratedExpr
  deriving (Show, Eq)

prettyOp :: BinaryOp -> String
prettyOp Equal = "=="
prettyOp Unequal = "!="
prettyOp LessThan = "<"
prettyOp LessThanEquals = "<="
prettyOp GreaterThan = ">"
prettyOp GreaterThanEquals = ">="
prettyOp Plus = "+"
prettyOp Minus = "-"
prettyOp Times = "*"
prettyOp Over = "/"
prettyOp Comma = ","
prettyOp Raise = "^"

parenthesize :: [String] -> String
parenthesize xs = "(" <> unwords xs <> ")"

prettyPrint :: Expr -> String
prettyPrint e = case expr e of
prettyPrint (Number n) = show n
prettyPrint (String s) = show s
prettyPrint (Boolean b) = if b then "true" else "false"
prettyPrint (Not e) = parenthesize [prettyPrint e]
prettyPrint (Negative e) = parenthesize ["-", prettyPrint e]
prettyPrint (Binary op e1 e2) = parenthesize [prettyOp op, prettyPrint e1, prettyPrint e2]
prettyPrint (Identifier name) = parenthesize ["identifier", name]
prettyPrint (Ternary cond true false) = parenthesize $ map prettyPrint [cond, true, false]
prettyPrint (FunctionCall f args) = parenthesize $ "function-call" : f : map prettyPrint args
