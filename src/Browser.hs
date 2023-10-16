module Browser ( buildAPI ) where

import Data.Maybe (isJust, fromJust)
import Data.List (intercalate)

import Components (ProgramParser, Interpreter)

buildAPI :: Show b => ProgramParser a -> Interpreter a b -> String -> String
buildAPI parse interpret contents = do
  case parse contents of
    Left err -> show err
    Right ast -> do
      let results = interpret ast
      case results of
        Left err -> show err
        Right outputs -> intercalate "\n" $ map (show . fromJust) (filter isJust outputs)
