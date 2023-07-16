module Main (main) where

import SystemF.Parser
-- import SystemF.Interpreter
-- import CLI
import Components
import System.Environment
import Data.Text (pack)

main :: IO ()
main = buildMain parse

buildMain :: Show a => ProgramParser a -> IO ()
buildMain parser = do
  args <- getArgs

  case parseArgs args of
    Nothing     -> putStrLn "Invalid arguments."
    (Just file) -> do
      contents <- readFile file

      case parser (pack contents) of
        Left err -> print err
        Right ast -> print ast

parseArgs :: [String] -> Maybe String
parseArgs [file] = Just file
parseArgs _      = Nothing
