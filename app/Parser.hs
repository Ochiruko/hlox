{-# LANGUAGE LambdaCase #-}
module Parser where

import Data.Void (Void)
import Data.Maybe (fromMaybe, fromJust)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Error as E

import qualified Ast as Ast
import qualified Control.Arrow as A
import Data.Functor (void, ($>))

type Parser = M.Parsec Void String

-- | parse (context|filename) sourceCode
parse :: String -> String -> Either String Ast.Expr
parse context = A.right fromJust
              . A.left E.errorBundlePretty
              . M.runParser (expr <* M.eof) context

-- | Returns the contents of the parsed string. Also consumes any following whitespace.
string :: Parser Ast.Expr
string = do
  _ <- C.char '"' <?> "valid string"
  contents <- M.many (M.noneOf ['"'])
  _ <- C.char '"'
  space
  pure . Ast.String $ contents

-- | Returns the double represented by the parsed number. Also consumes any following whitespace.
number :: Parser Ast.Expr
number = do
  whole <- M.some C.numberChar <?> "valid double"
  mFractional <- M.optional $ do
    _ <- C.char '.'
    M.some C.numberChar <?> "non-empty fractional component"
  space
  pure . Ast.Number . read $ whole <> "." <> fromMaybe "0" mFractional

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "//") M.empty

boolean :: Parser Ast.Expr
boolean = M.choice
  [ Ast.Boolean True <$ C.string "true"
  , Ast.Boolean False <$ C.string "false"
  ] <* space

-- lexeme :: String -> Parser String
-- lexeme = L.lexeme

-- | will fail if a keyword is read. Consider refactoring to use a better failure method.
identifier :: Parser Ast.Expr
identifier = Ast.Identifier <$> identifier_ <* space

-- | Does not skip following whitespace.
identifier_ :: Parser String
identifier_ = do
   word <- (:) <$> C.letterChar <*> M.many C.alphaNumChar
   case elem word keywordsList of
     True -> fail $ show word <> " is a keyword. Please use a different name."
     False -> pure word

maxBinaryPrec :: Int
maxBinaryPrec = length binaryOpsPrecTable - 1

-- | Operations should be parsed in order of precedence from low to high, e.g.
-- (a + b) == (b + c) where (==) < (+).
binaryOpsPrecTable :: [[(String, Ast.BinaryOp)]]
binaryOpsPrecTable = [[ (",", Ast.Comma) ],

                     [], -- Note that this level is left free for the ternary operator.

                     [ ("==", Ast.Equal)
                     , ("!=", Ast.Unequal) ],

                     [ ("<=", Ast.LessThanEquals)
                     , (">=", Ast.GreaterThanEquals)
                     , ("<", Ast.LessThan)
                     , (">", Ast.GreaterThan) ],

                     [ ("-", Ast.Minus)
                     , ("+", Ast.Plus) ],

                     [ ("*", Ast.Times)
                     , ("/", Ast.Over) ],
                    
                     [ ("^", Ast.Raise) ]]

data Fixity = FixLeft | FixRight deriving (Eq, Show)

precFixity :: [(Int, Fixity)]
precFixity = [ (0, FixLeft)
             , (1, FixLeft)
             , (2, FixLeft)
             , (3, FixLeft)
             , (4, FixLeft)
             , (5, FixLeft)
             , (6, FixRight)
             ]

-- | parses any valid function argument. Note that this language excludes tuples
-- for some ridiculous reason. I don't believe this ever fails.
argumentExpr :: Parser Ast.Expr
argumentExpr = recoverToWith toNextExpr fallbackExpr (exprPrec 1)

-- | Parses any valid lox expression. Refer to the formal grammar for details.
-- Note that this should always fail further down.
-- TODO: Write a formal grammar.
expr :: Parser Ast.Expr
expr = exprPrec 0
  

-- | Parses the symbol of a binary operation with precedence prec.
-- Note that because all parsers in this module skip comments with space,
-- a comment before should have been eliminated. Also note that this is
-- far from efficient.
--
binaryOpPrec :: Int -> Parser Ast.BinaryOp
binaryOpPrec prec
  | prec > maxBinaryPrec = error "invalid precedence"
  | otherwise = do
      let fromAssocList = map (\(str, ast) -> ast <$ C.string str) 
      -- I want to use the default label, which is a list of the binary operations
      -- of precedence `prec`.
      op <- M.choice . fromAssocList $ (binaryOpsPrecTable !! prec)
      space
      pure op

toNextExpr :: Parser ()
toNextExpr = M.choice
  [ void (C.char ';')
  , M.lookAhead keyword ]

toNextBinaryOp :: Parser ()
toNextBinaryOp =
  let anyBinaryOp = "?" : ":" : map fst (concat binaryOpsPrecTable)
  in (void . M.lookAhead . M.choice . map C.string $ anyBinaryOp) <|> toNextExpr

-- An arbitrary fallback/dud for when the parse is going to fail anyways.
fallbackExpr :: Ast.Expr
fallbackExpr = Boolean False

-- | Parses into lists by precedence. Associates left: (a + b) + c
--
-- Returns Nothing instead of failing.
-- 
-- And Done!
exprPrec :: Int -> Parser Ast.Expr
exprPrec prec
  -- Throw the error up
  | prec > maxBinaryPrec = unary
  -- Handle errors right here
  | prec == 1 = ternary
  | otherwise = do
      -- The try is implicit
      headSubExpr <- recoverToWith toNextBinaryOp (Ast.Boolean False) $ exprPrec (prec + 1)
      opsAndTailSubExprs <- M.many $ do
        op <- binaryOpPrec prec
        mSubExpr <- recoverToWith toNextBinaryOp fallbackExpr $ exprPrec (prec - 1)
        pure (op, mSubExpr)
      case lookup prec precFixity of
        Just FixLeft ->
          let buildBinaryExprL left (op, right) = Ast.Binary op left right
          in pure $ foldl' buildBinaryExprL headSubExpr opsAndTailSubExprs
        Just FixRight ->
          let binaryExprR left ((op, right) : rest) =
                Ast.Binary op left $ binaryExprR right rest
              binaryExprR mLeft [] = mLeft
          in pure $ binaryExprR headSubExpr opsAndTailSubExprs
        Nothing -> error "impossible, we already checked prec"

-- | If there is no ternary expression, this just returns the proposition.
-- Has very complete error handling.
--
-- One way to refactor this might be to have '?' emit an intermediate newtype
-- over expr, and then do a miniature type check on that newtype at ':'.
ternary :: Parser Ast.Expr
ternary = do
  mProp <- recoverToWith toNextBinaryOp fallbackExpr $ exprPrec 2   
  eitherRoutes <- do
    eitherMTrueRoute <- do
      -- this line forces the parser to either fail or commit
      eitherError <- M.observing $ C.char '?' *> space
      trueExpr <- recoverToWith toNextBinaryOp Nothing (exprPrec 2)
      pure $ eitherError $> trueExpr
    mFalseRoute <- M.optional $ do
      C.char ':' *> space
      case eitherMTrueRoute of
        -- This should really be given a more explanatory, labeled error message.
        Left err -> M.registerParseError err
        _ -> pure ()
      recoverToWith toNextBinaryOp Nothing $ exprPrec 1
    case (eitherMTrueRoute, mFalseRoute) of
      (Right (Just trueRoute), Just (Just falseRoute)) ->
        pure $ Right (trueRoute, falseRoute)
      (Left _, Nothing) -> pure $ Left True -- no input was consumed, valid result.
      _ -> pure $ Left False -- some input was consumed, invalid result.
  pure $ do
    prop <- mProp
    case eitherRoutes of
      Right (trueRoute, falseRoute) -> Just $ Ast.Ternary prop trueRoute falseRoute
      Left True -> Just prop
      Left False -> Nothing

    

unary :: Parser Ast.Expr
unary = do
  ops <- M.many $ (C.char '!' <|> C.char '-') <* space
  subExpr <- primitive
  let buildUnaryExpr '!' acc = Ast.Not acc
      buildUnaryExpr '-' acc = Ast.Negative acc
      buildUnaryExpr _ _ = error "impossible pattern match"
  pure $ foldr buildUnaryExpr subExpr ops

-- | parse a parenthesized expr or primitive.
primitive :: Parser Ast.Expr
primitive = M.choice [ number
                     , string
                     , boolean
                     , functionCallOrIdentifier 
                     , parens expr ]
                            

functionCallOrIdentifier :: Parser Ast.Expr
functionCallOrIdentifier = do
  name <- identifier_ -- Add "*> space" if space should be allowed between.
  mArgs <- M.optional . parens $ M.sepBy argumentExpr (C.char ',' *> space)
  space
  pure $ case mArgs of
    Just args -> Ast.FunctionCall name args
    Nothing -> Ast.Identifier name

-- | surrounds parser in parens. Skips space between the first
-- paren and the parser.
parens :: Parser a -> Parser a
parens parser = C.char '(' *> space *> parser <* C.char ')'

-- | If p fails, then skip until end succeeds. Wrap in M.lookAhead if
-- consumption of end is not desirable.
-- TODO: Spread this throughout the program
recoverToWith :: Parser b -> a -> Parser a -> Parser a
recoverToWith recoveryPoint fallBack p = M.withRecovery recover p
  where recover err = do
          M.registerParseError err
          _ <- M.manyTill M.anySingle (M.lookAhead recoveryPoint)
          pure fallBack

keyword :: Parser ()
keyword = () <$ M.choice (map C.string keywordsList)
    
keywordsList :: [String]
keywordsList = [ "class"
               , "fun"
               , "var"
               , "for"
               , "if"
               , "while"
               , "print"
               , "return" ]
