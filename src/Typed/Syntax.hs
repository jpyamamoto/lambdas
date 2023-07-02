module Typed.Syntax ( Term (..)
                    , Instruction (..)
                    , Result (..)
                    , Type (..)
                    ) where

import Text.Megaparsec (unPos, Pos)

{-
  Locally nameless representation:
  `Var Nothing _` is used for variables whose value
  may be defined in the context. We no longer accept
  Free variables, as they cannot be typed.

  `Var (Just _) _` is used for Bound Variables.
-}

data Term =
  -- Basic Types
    Nat Int
  | Bool Bool
  -- Lambda Calculus Constructions
  | Var (Maybe Int) String -- Locally nameless representation
  | Abs String Type Term
  | App Term Term
  -- Destructors
  | If Term Term Term
  | IsZero Term
  -- Operators
  | Suc Term
  | Add Term Term
  | Mul Term Term

instance Show Term where
  show (Nat n)      = show n
  show (Bool True)  = "true"
  show (Bool False) = "false"
  show (Var _ n)    = n
  show (Abs n ty t) = "(λ " ++ n ++ " : " ++ show ty ++ " . " ++ show t ++ ")"
  show (App t1 t2)  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (If g t e)   = unwords $ "if" : map show [g, t, e]
  show (IsZero t)   = "(iszero " ++ show t ++ ")"
  show (Suc t)      = "(succ " ++ show t ++ ")"
  show (Add l r)    = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Mul l r)    = "(" ++ show l ++ " * " ++ show r ++ ")"

data Type =
    Natural
  | Boolean
  | Arrow Type Type
  deriving Eq

instance Show Type where
  show Natural = "ℕ"
  show Boolean = "𝔹"
  show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

data Instruction b =
    Eval Pos b
  | Info Pos b
  | Type Pos b
  | Defn String b

instance Show b => Show (Instruction b) where
  show (Eval _ t) = ":e " ++ show t
  show (Info _ t) = ":i " ++ show t
  show (Type _ t) = ":t " ++ show t
  show (Defn n t) = n ++ " ≐ " ++ show t

data Result = ResTerm (Pos, Term) | ResType (Pos, Type)

instance Show Result where
  show (ResTerm (p, t)) = "Output [Line " ++ show (unPos p) ++ "]: " ++ show t
  show (ResType (p, t)) = "Type [Line " ++ show (unPos p) ++ "]: " ++ show t
