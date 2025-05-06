{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}

module Lexical (lexicalErrors) where

import Data.Set (Set, member)
import Data.Text (Text, pattern Empty, pattern (:<))
import Control.Monad.Writer.CPS
import qualified Data.Text as T

data ParseState = ParseState
  { line :: Int
  , remaining :: Text
  }

lexicalErrors :: Text -> [String]
lexicalErrors = reverse . execWriter . untilEof scanToken . initializeState

untilEof :: Monad m => (ParseState -> m ParseState) -> ParseState -> m ParseState
untilEof f pstate
  | T.null (remaining pstate) = pure pstate
  | otherwise = f pstate >>= untilEof f
  

-- lexicalErrors = 
--   (Writer [String] ParseState -> [String])
--   .  ((ParseState -> Writer ParseState) -> [String] -> Writer [String] ParseState)
--        (ParseState -> Writer ParseState)

initializeState :: Text -> ParseState
initializeState t = ParseState { line = 1, remaining = t }

data ScanResult
  = TryNext
  | Stop
  deriving (Eq, Show)
  
scanToken :: ParseState -> Writer [String] ParseState
scanToken = choice
  [ string
  , number
  , symbol
  , atom
  , eof -- This should return PartialSuccess on failure.
  , unrecognizedChar
  ]

-- | Choice should take the first consumptive pstate
choice :: [ParseState -> Writer [String] (ScanResult, ParseState)]
       -> ParseState
       -> Writer [String] ParseState
choice = undefined

symbol, atom, eof 
  :: ParseState -> Writer [String] (ScanResult, ParseState)

string :: ParseState -> Writer [String] (ScanResult, ParseState)
string pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = case take1 pstate of
      ('"', afterFirstQuote) -> error "Hasn't been implemented yet" afterFirstQuote 
      (c, pstate') -> do tell [report (line pstate') "Expected a quote, found " <> show c]
                         pure (TryNext, pstate)
                                 
number :: ParseState -> Writer [String] (ScanResult, ParseState)
number pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = case take1 pstate of
      (firstDigit, pstate')
        | firstDigit `member` ['0'..'9'] ->
            let (restOfWholeDigits, pstate'') = many digit pstate' in
              undefined
      (c, _) -> do tell [report (line pstate) $ "Expected a digit, found " <> show c]
                   pure (TryNext, pstate)
      
symbol = undefined
atom = undefined
eof = undefined

unrecognizedChar :: ParseState -> Writer [String] (ScanResult, ParseState)
unrecognizedChar pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = do tell [report (line pstate') "Unrecognized character: " <> show c]
                   pure (Stop, pstate')
                     where (c, pstate') = take1 pstate

digit :: Matchable
digit = OneOf ['0'..'9']

alpha :: Matchable
alpha = OneOf $ ['a'..'z'] <> ['A'..'Z']

alphaNum :: Matchable
alphaNum = OneOf $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z']

-- | Partial function, refactor later?
take1 :: ParseState -> (Char, ParseState)
take1 pstate = case remaining pstate of
  Empty -> error "This function does not accept completed pstates."
  '\n' :< cs -> ('\n', ParseState { line = line pstate + 1
                                  , remaining = cs })
  c :< cs -> (c, pstate { remaining = cs })

-- | Parses zero or more chars in cs.
many :: Matchable -> ParseState -> ([Char], ParseState)
many matchable pstate
  | T.null (remaining pstate) = ([], pstate)
  | otherwise = let (c, pstate') = take1 pstate
                    (rest, pstate'') = many matchable pstate'
                in if match matchable c
                   then (c : rest, pstate'')
                   else ([], pstate)

data Matchable
  = NoneOf (Set Char)
  | OneOf (Set Char)
  deriving (Eq, Ord)

match :: Matchable -> Char -> Bool
match (NoneOf cs) c = c `notElem` cs
match (OneOf cs) c = c `elem` cs

report :: Int -> String -> String
report line msg = "[" <> show line <> "]" <> msg
