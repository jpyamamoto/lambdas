module Main (main) where

import Typed.Parser
-- import Typed.Interpreter
-- import CLI

-- TODO: Remove eventually
import System.Environment
import Data.Text (pack)


parseArgs :: [String] -> Maybe String
parseArgs [file] = Just file
parseArgs _      = Nothing

main :: IO ()
main = do
  args <- getArgs

  case parseArgs args of
    Nothing     -> putStrLn "Invalid arguments."
    (Just file) -> do
      contents <- readFile file

      case parse (pack contents) of
        Left err -> putStrLn err
        Right ast -> print ast
