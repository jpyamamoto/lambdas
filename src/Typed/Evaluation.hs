module Typed.Evaluation ( eval
                        , Context
                        ) where

import qualified Data.Map.Strict as M

import Typed.Syntax

type Context = M.Map String Term

eval :: Context -> Term -> Term
eval ctx t = case beta ctx t of
  (Just t') -> eval ctx t'
  Nothing   -> t


beta :: Context -> Term -> Maybe Term
beta ctx (Var Nothing name) = M.lookup name ctx
beta ctx (App (Abs n ty t) x)
  | isVal ctx x = Just $ shift (-1) 0 (substitute t 0 (shift 1 0 x))
  | otherwise   = beta ctx x >>= Just . App (Abs n ty t)
-- To reduce body of abstractions
beta ctx (Abs n ty b) = beta ctx b >>= Just . Abs n ty
beta ctx (App f x)  = beta ctx f >>= \t' -> Just (App t' x)
beta _   (If (Bool True) t _) = Just t
beta _   (If (Bool False) _ e) = Just e
beta ctx (If g t e) = beta ctx g >>= \g' -> Just (If g' t e)
beta _   (IsZero (Nat n)) = Just $ Bool (n == 0)
beta ctx (IsZero n) = beta ctx n >>= Just . IsZero
beta _   (Suc (Nat n)) = Just $ Nat (n + 1)
beta ctx (Suc n)
  | not (isVal ctx n) = beta ctx n >>= Just . Suc
beta _   (Add (Nat n) (Nat m))  = Just $ Nat (n + m)
beta ctx (Add n m)
  | isVal ctx n = beta ctx m >>= Just . Add n
  | otherwise   = beta ctx n >>= \n' -> Just (Add n' m)
beta _   (Mul (Nat n) (Nat m))  = Just $ Nat (n * m)
beta ctx (Mul n m)
  | isVal ctx n = beta ctx m >>= Just . Mul n
  | otherwise   = beta ctx n >>= \n' -> Just (Mul n' m)
beta _ _ = Nothing


isVal :: Context -> Term -> Bool
isVal _   (Nat _)     = True
isVal _   (Bool _)    = True
isVal ctx (Var Nothing name) = not $ M.member name ctx
isVal _   (Var _ _)   = True
isVal _   (Abs _ _ _) = True
isVal _   _           = False


shift :: Int -> Int -> Term -> Term
shift off cut (Var (Just i) n)
  | i < cut                = Var (Just i) n
  | otherwise              = Var (Just (i + off)) n
shift off cut (Abs n ty t) = Abs n ty $ shift off (cut + 1) t
shift off cut (App f x)    = App (shift off cut f)
                                 (shift off cut x)
shift off cut (If g t e)   = If (shift off cut g)
                                (shift off cut t)
                                (shift off cut e)
shift off cut (IsZero t)   = IsZero (shift off cut t)
shift off cut (Suc n)      = Suc (shift off cut n)
shift off cut (Add l r)    = Add (shift off cut l)
                                 (shift off cut r)
shift off cut (Mul l r)    = Mul (shift off cut l)
                                 (shift off cut r)
shift _      _      t      = t


substitute :: Term -> Int -> Term -> Term
substitute (Var (Just i) n) index t
  | i == index                    = t
  | otherwise                     = Var (Just i) n
substitute (Abs n ty t1) index t2 = Abs n ty $
                                        substitute t1 (index + 1) (shift 1 0 t2)
substitute (App f x)  index t2    = App (substitute f index t2)
                                        (substitute x index t2)
substitute (If g t e) index t2    = If (substitute g index t2)
                                       (substitute t index t2)
                                       (substitute e index t2)
substitute (IsZero t) index t2    = IsZero (substitute t index t2)
substitute (Suc t)    index t2    = Suc (substitute t index t2)
substitute (Add l r)  index t2    = Add (substitute l index t2)
                                        (substitute r index t2)
substitute t          _     _     = t
