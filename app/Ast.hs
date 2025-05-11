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

module Ast where

-- | Type checking is delegated to evaluation.
-- Note that every operator ~ should be parsed as left-associative, (x ~ y) ~ z.
data BinaryOp = Equal
              | Unequal
              | LessThan
              | LessThanEquals
              | GreaterThan
              | GreaterThanEquals
              | Plus
              | Minus
              | Times
              | Slash
              deriving (Show, Eq)

data Expr = Number Double
          | String String
          | Not Expr
          | Negative Expr
          | Binary BinaryOp Expr Expr
          | Parens Expr
          deriving (Show, Eq)
