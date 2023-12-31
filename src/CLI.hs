module CLI ( buildMain ) where

import System.Environment
import Data.Maybe (isJust, fromJust)

import Components (ProgramParser, Interpreter)

buildMain :: Show b => ProgramParser a -> Interpreter a b -> IO ()
buildMain parse interpret = do
  args <- getArgs

  case parseArgs args of
    Nothing     -> putStrLn "Invalid arguments."
    (Just file) -> do
      contents <- readFile file

      case parse contents of
        Left err -> print err
        Right ast -> do
          let results = interpret ast
          case results of
            Left err -> print err
            Right outputs -> mapM_ (print . fromJust) $ filter isJust outputs

parseArgs :: [String] -> Maybe String
parseArgs [file] = Just file
parseArgs _      = Nothing
