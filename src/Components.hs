module Components ( ProgramParser
                  , Interpreter
                  ) where

import Data.Text (Text)

import Error

type ProgramParser a = Text -> Either Error [a]

type Interpreter a b = [a] -> Either Error [Maybe b]
