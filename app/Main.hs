module Main where

import Control.Monad (forever, when)
import Data.Functor (($>))
import qualified Lexical as L
import qualified Parser as P
import qualified Ast
import System.Environment (getArgs)
import System.Exit
import qualified System.IO as IO -- (IOMode (ReadMode), hFlush, hPutStrLn, stderr, stdout, withFile)
import Prelude hiding (getLine)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runPrompt >> exitSuccess
    1 -> runFile (args !! 1)
    _ -> do
      IO.hPutStrLn IO.stderr "usage: hlox [script]"
      exitFailure

-- | Exit point. Open a repl in stdin and stdout. C-c and C-d are also capable
-- of halting execution because we use GHC's execution defaults.
runPrompt :: IO ()
runPrompt = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  line <- IO.getLine
  when (line == ":q") exitSuccess
  run "REPL" line $> ()

runFile :: String -> IO ()
runFile fileName = IO.withFile fileName IO.ReadMode $ \h -> do
  sourceCode <- IO.hGetContents h
  validSource <- run fileName sourceCode
  if validSource
    then exitSuccess
    else exitFailure

-- | The output style isn't particularly idiomatic because it mixes effects with
-- logic, but it works and that's the number one priority right now. Returns True
-- if the source code is valid.
run :: String -> String -> IO Bool
run context source = 
  let lexicalErrors = L.lexicalErrors source
      (validSource, parseResult) = case P.parse context source of
        Left parseErrors -> (False, parseErrors)
        Right ast -> (True, Ast.prettyPrint ast)
  in case lexicalErrors of
    [] -> putStrLn parseResult *> pure True
    errors -> putStrLn (unlines errors) *> pure validSource
    
