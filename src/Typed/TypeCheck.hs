module Typed.TypeCheck ( typeOf
                       , Context
                       ) where

import Error
import Typed.Syntax

type Context = [(String, Type)]

typeOf :: Context -> Term -> Either Error Type
typeOf _   (Nat _)     = Right Natural
typeOf _   (Bool _)    = Right Boolean
typeOf ctx (Var (Just i) n) = typeFromContext ctx n i
typeOf ctx (Var Nothing  n) = typeFromContext ctx n 0
typeOf ctx (Abs v t b) = typeOf ((v,t):ctx) b >>= Right . Arrow t
typeOf ctx (App f x)   = do
  tyf <- typeOf ctx f
  tyx <- typeOf ctx x
  case tyf of
    (Arrow tya tyr) -> if tya == tyx
      then Right tyr
      else typeErr $ show x ++ " cannot be applied to type " ++ show tyf
    _               -> typeErr $ show x ++ " cannot be applied to type " ++ show tyf
typeOf ctx (If g t e)  = do
  tyg <- typeOf ctx g
  tyt <- if tyg == Boolean
    then typeOf ctx t
    else typeErr $ show t ++ " cannot be used in `then` branch of `if` (is not a natural)"
  tye <- typeOf ctx e
  if tyt == tye
    then Right tyt
    else typeErr "`then` and `else` branches in `if` have different types"
typeOf ctx (IsZero t)  = do
  ty <- typeOf ctx t
  if ty == Natural
    then Right Boolean
    else typeErr $ show t ++ " cannot be used as argument to `iszero` (is not a natural)"
typeOf ctx (Suc t)     = do
  ty <- typeOf ctx t
  if ty == Natural
    then Right Natural
    else typeErr $ show t ++ " cannot be used as argument to `suc` (is not a natural)"
typeOf ctx (Add l r)   = do
  ty1 <- typeOf ctx l
  ty2 <- if ty1 == Natural
    then typeOf ctx r
    else typeErr $ show l ++ " cannot be used as argument to `+` (is not a natural)"
  if ty2 == Natural
    then Right Natural
    else typeErr $ show r ++ " cannot be used as argument to `+` (is not a natural)"
typeOf ctx (Min l r)   = do
  ty1 <- typeOf ctx l
  ty2 <- if ty1 == Natural
    then typeOf ctx r
    else typeErr $ show l ++ " cannot be used as argument to `-` (is not a natural)"
  if ty2 == Natural
    then Right Natural
    else typeErr $ show r ++ " cannot be used as argument to `-` (is not a natural)"
typeOf ctx (Mul l r)   = do
  ty1 <- typeOf ctx l
  ty2 <- if ty1 == Natural
    then typeOf ctx r
    else typeErr $ show l ++ " cannot be used as argument to `+` (is not a natural)"
  if ty2 == Natural
    then Right Natural
    else typeErr $ show r ++ " cannot be used as argument to `+` (is not a natural)"
typeOf ctx (Fix t)     = do
  ty1 <- typeOf ctx t
  case ty1 of
    (Arrow tyf tyx) -> if tyf == tyx
                       then return tyf
                       else typeErr $ show ty1 ++ " cannot be type of argument to `fix`"
    _               -> typeErr $ "Type of " ++ show ty1 ++ " has to be a function"

typeErr :: String -> Either Error Type
typeErr = Left . TypeError

typeFromContext :: Context -> String -> Int -> Either Error Type
typeFromContext []  name _ = typeErr $ name ++ " has an invalid type"
typeFromContext ctx name 0 = case lookup name ctx of
  (Just ty) -> Right ty
  Nothing   -> typeErr $ name ++ " has an invalid type"
typeFromContext ctx name i = typeFromContext (drop i ctx) name 0
