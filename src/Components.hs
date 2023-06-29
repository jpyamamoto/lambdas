module Components ( ProgramParser
                  , Interpreter
                  , Error (..)
                  ) where

import Data.Text (Text)

data Error =
    ParseError String
  | EvalError String

instance Show Error where
  show (ParseError msg) = "Parse Error - " ++ show msg
  show (EvalError  msg) = "Evaluation Error - " ++ show msg

type ProgramParser a = Text -> Either Error [a]

type Interpreter a b = [a] -> Either Error [Maybe b]
