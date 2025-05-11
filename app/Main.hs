{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever, when)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO (getLine, hGetContents)
import Lexical
import System.Environment (getArgs)
import System.Exit
import System.IO (IOMode (ReadMode), hFlush, hPutStrLn, stderr, stdout, withFile)
import Prelude hiding (getLine)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runPrompt >> exitSuccess
    1 -> runFile (args !! 1)
    _ -> do
      hPutStrLn stderr "usage: hlox [script]"
      exitFailure

-- | Exit point. Open a repl in stdin and stdout. C-c and C-d are also capable
-- of halting execution because we use GHC's execution defaults.
runPrompt :: IO ()
runPrompt = forever $ do
  putStr "> "
  hFlush stdout
  line <- getLine
  when (line == ":q") exitSuccess
  run line $> ()

runFile :: String -> IO ()
runFile fileName = withFile fileName ReadMode $ \h -> do
  sourceCode <- hGetContents h
  validResult <- run sourceCode
  if validResult
    then exitSuccess
    else exitFailure

-- | The output style isn't particularly idiomatic because it mixes effects with
-- logic, but it works and that's the number one priority right now.
run :: Text -> IO Bool
run source = case lexicalErrors source of
  [] -> putStrLn "All tokens are well-formed." $> True
  errors -> putStrLn (unlines errors) $> False
