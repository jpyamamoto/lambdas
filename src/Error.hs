module Error ( Error (..) ) where

data Error =
    ParseError String
  | EvalError String
  | TypeError String

instance Show Error where
  show (ParseError msg) = "Parse Error - " ++ msg
  show (EvalError  msg) = "Evaluation Error - " ++ msg
  show (TypeError  msg) = "Typing Error - " ++ msg
