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

data Expr = Number Double
          | String String
          | Identifier String
          | Ternary Expr Expr Expr
          | Boolean Bool
          | Not Expr
          | Negative Expr
          | Binary BinaryOp Expr Expr
          | Parens Expr
          | FunctionCall String [Expr]
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
prettyPrint (Number n) = show n
prettyPrint (String s) = show s
prettyPrint (Boolean b) = if b then "true" else "false"
prettyPrint (Not e) = parenthesize [prettyPrint e]
prettyPrint (Negative e) = parenthesize ["-", prettyPrint e]
prettyPrint (Binary op e1 e2) = parenthesize [prettyOp op, prettyPrint e1, prettyPrint e2]
prettyPrint (Parens e) = parenthesize ["parens", prettyPrint e]
prettyPrint (Identifier name) = parenthesize ["identifier", name]
prettyPrint (Ternary cond true false) = parenthesize $ map prettyPrint [cond, true, false]
prettyPrint (FunctionCall f args) = parenthesize $ "function-call" : f : map prettyPrint args
