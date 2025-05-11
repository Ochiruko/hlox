{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Lexical where

import Control.Monad (foldM)
import Control.Monad.Writer.CPS
import Data.Set (Set, member)
import Data.Text (Text, pattern Empty, pattern (:<))
import Data.Text qualified as T

data ParseState = ParseState
  { line :: Int,
    offset :: Int,
    remaining :: Text
  }
  deriving (Show, Eq)

lexicalErrors :: Text -> [String]
lexicalErrors = reverse . execWriter . untilEof scanToken . initializeState

untilEof :: (Monad m) => (ParseState -> m ParseState) -> ParseState -> m ParseState
untilEof f pstate
  | T.null (remaining pstate) = pure pstate
  | otherwise = f pstate >>= untilEof f

initializeState :: Text -> ParseState
initializeState t = ParseState {line = 1, offset = 0, remaining = t}

data ScanResult
  = TryNext
  | Stop
  deriving (Eq, Show)

scanToken :: ParseState -> Writer [String] ParseState
scanToken =
  choice
    [ space,
      string,
      number,
      symbolOrComment,
      atom,
      eof, -- This should return PartialSuccess on failure.
      unrecognizedChar
    ]

-- | On a TryNext, the next parser in the parser list should be applied to
-- the unmodified initial state. If the last one is TryNext, the result is
-- undefined. The return value should be the first Stop parser's result.
-- "Success" is irrelevant to this parser.
choice ::
  [ParseState -> Writer [String] (ScanResult, ParseState)] ->
  ParseState ->
  Writer [String] ParseState
choice parsers pstate = snd <$> foldM maybeTryParser (TryNext, pstate) parsers
  where
    maybeTryParser (Stop, finalPstate) _ = pure (Stop, finalPstate)
    maybeTryParser (TryNext, currentPstate) currentParser = currentParser currentPstate

-- | equivalent to r"\"([^"]*)\"".
string :: ParseState -> Writer [String] (ScanResult, ParseState)
string pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = case take1 pstate of
      ('"', afterFirstQuote) ->
        let (_, afterContents) = many (NoneOf ['"']) afterFirstQuote
         in if T.null (remaining afterContents)
              then do
                tell [report afterContents "Expected a quote, found EOF"]
                pure (Stop, afterContents)
              else case take1 afterContents of
                ('"', afterLastQuote) -> pure (Stop, afterLastQuote)
                (c, pstate') -> do
                  tell [report pstate' "Expected a double-quote, found " <> show c]
                  pure (Stop, afterContents)
      _ -> do
        pure (TryNext, pstate)

-- | equivalent to r"\d+(\.\d+)?"
number :: ParseState -> Writer [String] (ScanResult, ParseState)
number pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = case take1 pstate of
      (firstDigit, afterFirstDigit)
        | match digit firstDigit ->
            let (_, afterWholePart) = many digit afterFirstDigit
             in if T.null (remaining afterWholePart)
                  then pure (Stop, afterWholePart)
                  else case take1 afterWholePart of
                    ('.', followedByFractionalPart) ->
                      if T.null (remaining followedByFractionalPart)
                        then do
                          tell [report followedByFractionalPart "Expected a digit, found EOF"]
                          pure (Stop, followedByFractionalPart)
                        else case take1 afterWholePart of
                          (firstFractionalDigit, followedByRestOfFractionalPart)
                            | match digit firstFractionalDigit -> do
                                let (_, rest) = many digit followedByRestOfFractionalPart
                                pure (Stop, rest)
                          (c, afterUnexpectedSymbol) -> do
                            tell [report afterUnexpectedSymbol $ "Expected a digit, found " <> show c]
                            pure (Stop, followedByFractionalPart)
                    _ -> pure (Stop, afterWholePart)
      _ -> pure (TryNext, pstate)

symbolOrComment :: ParseState -> Writer [String] (ScanResult, ParseState)
symbolOrComment pstate
  | T.null (remaining pstate) = do
      -- tell [report pstate "Expected a letter, found EOF"]
      pure (TryNext, pstate)
  | otherwise = case take1 pstate of
      -- One letter symbols
      ('(', pstate') -> pure (Stop, pstate')
      (')', pstate') -> pure (Stop, pstate')
      ('{', pstate') -> pure (Stop, pstate')
      ('}', pstate') -> pure (Stop, pstate')
      (',', pstate') -> pure (Stop, pstate')
      ('.', pstate') -> pure (Stop, pstate')
      ('-', pstate') -> pure (Stop, pstate')
      ('+', pstate') -> pure (Stop, pstate')
      (';', pstate') -> pure (Stop, pstate')
      ('*', pstate') -> pure (Stop, pstate')
      -- Two letter symbols
      ('/', pstate')
        | T.null (remaining pstate') -> pure (Stop, pstate')
        | otherwise -> case take1 pstate' of
            ('/', pstate'') ->
              let (_, afterComment) = many sameLine pstate''
               in pure (Stop, afterComment)
            _ -> pure (Stop, pstate')
      ('!', pstate') -> pure (Stop, optional (OneOf ['=']) pstate')
      ('=', pstate') -> pure (Stop, optional (OneOf ['=']) pstate')
      ('>', pstate') -> pure (Stop, optional (OneOf ['=']) pstate')
      ('<', pstate') -> pure (Stop, optional (OneOf ['=']) pstate')
      (c, pstate') -> do
        -- tell [report pstate' $ "Expected a valid symbol, found " <> show c]
        pure (TryNext, pstate)

-- | equivalent to r"[a-zA-Z][0-9a-zA-Z]"
atom :: ParseState -> Writer [String] (ScanResult, ParseState)
atom pstate
  | T.null (remaining pstate) = do
      -- tell [report pstate "Expected a letter, found EOF"]
      pure (TryNext, pstate)
  | otherwise = case take1 pstate of
      (c, pstate')
        | match alpha c ->
            let (_, pstate'') = many alphaNum pstate'
             in pure (Stop, pstate'')
        | otherwise -> do
            -- tell [report pstate' $ "Expected a letter, found " <> show c]
            pure (TryNext, pstate)

space :: ParseState -> Writer [String] (ScanResult, ParseState)
space pstate
  | T.null (remaining pstate) = do
      -- tell [report pstate "Expected whitespace, found EOF"]
      pure (TryNext, pstate)
  | otherwise = case take1 pstate of
      (c, pstate')
        | match spaces c -> pure (Stop, pstate')
        | otherwise -> do
            -- tell [report pstate' "Expected whitespace, found " <> show c]
            pure (TryNext, pstate)

-- | equivalent to ""
eof :: ParseState -> Writer [String] (ScanResult, ParseState)
eof pstate
  | T.null (remaining pstate) = pure (Stop, pstate)
  | otherwise = do
      let (c, pstate') = take1 pstate
      -- tell [report pstate' $ "Expected EOF, found " <> show c]
      pure (TryNext, pstate)

unrecognizedChar :: ParseState -> Writer [String] (ScanResult, ParseState)
unrecognizedChar pstate
  | T.null (remaining pstate) = error "eof should prevent this from happening"
  | otherwise = do
      tell [report pstate' "Unrecognized character: " <> show c]
      pure (Stop, pstate')
  where
    (c, pstate') = take1 pstate

digit :: Matchable
digit = OneOf ['0' .. '9']

alpha :: Matchable
alpha = OneOf $ ['a' .. 'z'] <> ['A' .. 'Z']

alphaNum :: Matchable
alphaNum = OneOf $ ['0' .. '9'] <> ['a' .. 'z'] <> ['A' .. 'Z']

sameLine :: Matchable
sameLine = NoneOf ['\n', '\r']

spaces :: Matchable
spaces = OneOf ['\n', '\r', ' ', '\t']

-- | Partial function, refactor later?
take1 :: ParseState -> (Char, ParseState)
take1 pstate = case remaining pstate of
  Empty -> error "This function does not accept completed pstates."
  '\n' :< cs ->
    ( '\n',
      ParseState
        { line = line pstate + 1,
          remaining = cs,
          offset = 0
        }
    )
  c :< cs -> (c, pstate {remaining = cs, offset = offset pstate + 1})

-- | Parses zero or more chars in cs.
-- FIX: This doesn't work...
many :: Matchable -> ParseState -> ([Char], ParseState)
many matchable pstate
  | T.null (remaining pstate) = ([], pstate)
  | otherwise =
      let (c, pstate') = take1 pstate
          (rest, pstate'') = many matchable pstate'
       in if match matchable c
            then (c : rest, pstate'')
            else ([], pstate)

-- | Tries to parse a matching char. does nothing on failure.
optional :: Matchable -> ParseState -> ParseState
optional matchable pstate
  | T.null (remaining pstate) = pstate
  | otherwise =
      let (c, pstate') = take1 pstate
       in if match matchable c
            then pstate'
            else pstate

data Matchable
  = NoneOf (Set Char)
  | OneOf (Set Char)
  deriving (Eq, Ord)

match :: Matchable -> Char -> Bool
match (OneOf cs) c = c `member` cs
match (NoneOf cs) c = not (c `member` cs)

report :: ParseState -> String -> String
report pstate msg =
  "["
    <> show (line pstate)
    <> ":"
    <> show (offset pstate)
    <> "] "
    <> msg
