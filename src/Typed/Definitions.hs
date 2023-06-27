module Typed.Definitions ( Term (..)
                         , Instruction (..)
                         , Result (..)
                         ) where

import Text.Megaparsec (unPos, Pos)

{-
  Locally nameless representation:
  `Var Nothing _` is used for Free Variables, or variables whose value
  may be defined in the context.

  `Var (Just _) _` is used for Bound Variables.
-}

data Term =
    Nat Int
  | Bool Bool
  | Var (Maybe Int) String -- Locally nameless representation
  | Abs String Term
  | App Term Term
  | If Term Term Term
  | IsZero Term
  | Suc Term

instance Show Term where
  show (Nat n)      = show n
  show (Bool True)  = "true"
  show (Bool False) = "false"
  show (Var _ n)    = n
  show (Abs n t)    = "(λ " ++ n ++ " . " ++ show t ++ ")"
  show (App t1 t2)  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (If g t e)   = unwords $ "if" : map show [g, t, e]
  show (IsZero t)   = "(iszero " ++ show t ++ ")"
  show (Suc t)      = "(succ " ++ show t ++ ")"

data Instruction b =
    Eval Pos b
  | Info Pos b
  | Defn String b

instance Show b => Show (Instruction b) where
  show (Eval _ t) = ":e " ++ show t
  show (Info _ t) = ":i " ++ show t
  show (Defn n t) = n ++ " ≐ " ++ show t

newtype Result = Result (Pos, Term)

instance Show Result where
  show (Result (p, t)) = "Output [Line " ++ show (unPos p) ++ "]: " ++ show t
