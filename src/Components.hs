module Components ( ProgramParser
                  , Interpreter
                  ) where

import Data.Text (Text)

type ProgramParser a = Text -> Either String [a]

type Interpreter a b = [a] -> [Maybe b]
