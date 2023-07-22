module SystemF.Syntax ( Term (..)
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
  -- Lambda Calculus Constructions
    Var (Maybe Int) String -- Locally nameless representation
  | Abs String Type Term
  | App Term Term
  -- Type Abstractions
  | AbsT String Term
  | ArgT Type

instance Show Term where
  show (Var _ n)    = n
  show (Abs n ty t) = "(λ " ++ n ++ " : " ++ show ty ++ " . " ++ show t ++ ")"
  show (App t1 t2)  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (AbsT n t)   = "(Λ " ++ n ++ " . " ++ show t ++ ")"
  show (ArgT ty)    = "[" ++ show ty ++ "]"

data Type =
    VarT (Maybe Int) String
  | Arrow Type Type
  | All String Type
  deriving Eq

instance Show Type where
  show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (VarT _ n) = n
  -- show (VarT x n) = "(VarT " ++ show x ++ " " ++ show n ++ ")"
  show (All n ty) = "(∀ " ++ n ++ " . " ++ show ty ++ ")"

data Instruction =
    Eval Pos Term
  | Info Pos Term
  | Type Pos Term
  | DefTerm String Term
  | DefType String Type

instance Show Instruction where
  show (Eval _ t) = ":e " ++ show t
  show (Info _ t) = ":i " ++ show t
  show (Type _ t) = ":t " ++ show t
  show (DefTerm n t) = n ++ " ≐ " ++ show t
  show (DefType n t) = n ++ " :: " ++ show t

data Result = ResTerm (Pos, Term) | ResType (Pos, Type)

instance Show Result where
  show (ResTerm (p, t)) = "Output [Line " ++ show (unPos p) ++ "]: " ++ show t
  show (ResType (p, t)) = "Type [Line " ++ show (unPos p) ++ "]: " ++ show t
