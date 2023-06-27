module CLI ( buildMain
           , buildInterpreter
           ) where

import System.Environment

import Data.Text (pack)
import Components (ProgramParser, Interpreter)
import Data.Maybe (isJust, fromJust, catMaybes)

buildMain :: Show b => ProgramParser a -> Interpreter a b -> IO ()
buildMain parse interpret = do
  args <- getArgs

  case parseArgs args of
    Nothing     -> putStrLn "Invalid arguments."
    (Just file) -> do
      contents <- readFile file

      case parse (pack contents) of
        Left err -> putStrLn err
        Right ast -> do
          let results = interpret ast
          mapM_ (print . fromJust) $ filter isJust results

parseArgs :: [String] -> Maybe String
parseArgs [file] = Just file
parseArgs _      = Nothing

buildInterpreter :: ProgramParser a -> Interpreter a b -> String -> Either String [b]
buildInterpreter parse interpret program = do
  case parse (pack program) of
    Left err -> Left err
    Right ast -> (Right . catMaybes . interpret) ast
