module SystemF.Evaluation ( eval
                          , Context
                          ) where

import qualified Data.Map.Strict as M

import SystemF.Syntax
import qualified SystemF.TypeCheck as Ty

type TermsContext = M.Map String Term
type TypesContext = M.Map String Term
type Context = (TermsContext, TypesContext)

eval :: Context -> Term -> Term
eval ctx t = case beta ctx t of
  (Just t') -> eval ctx t'
  Nothing   -> t


beta :: Context -> Term -> Maybe Term
beta ctx (Var Nothing name) = M.lookup name (fst ctx)
beta _   (App (AbsT _ t) (ArgT ty)) = Just $ substTopTermType t ty
beta ctx (App (Abs _ _ t) x)
  | isVal ctx x = Just $ substTopTerm t x
beta ctx (App f x)
  | isVal ctx f = beta ctx x >>= Just . App f
beta ctx (App f x)  = beta ctx f >>= \f' -> Just (App f' x)
beta _ _        = Nothing


substTopTerm :: Term -> Term -> Term
substTopTerm t x = shift (-1) (substitute t 0 (shift 1 x))


substTopTermType :: Term -> Type -> Term
substTopTermType t ty = shift (-1) (substTermType t 0 (Ty.shift 1 ty))


isVal :: Context -> Term -> Bool
isVal _   (Abs _ _ _) = True
isVal _   (AbsT _ _)  = True
isVal _   _           = False


shift :: Int -> Term -> Term
shift off = shiftAbove off 0


shiftAbove :: Int -> Int -> Term -> Term
shiftAbove off = mapTerm onVar onType
  where onVar c i n
          | i < c     = Var (Just i) n
          | otherwise = Var (Just (i + off)) n
        onType = Ty.shiftAbove off


substitute :: Term -> Int -> Term -> Term
substitute t j s = mapTerm onVar onType j t
  where onVar index i n
          | i == index = shift index s
          | otherwise  = Var (Just i) n
        onType _ ty = ty


substTermType :: Term -> Int -> Type -> Term
substTermType t j ty = mapTerm onVar onType j t
  where onVar _ i = Var (Just i)
        onType j' tyTerm = Ty.substitute tyTerm j' ty


mapTerm :: (Int -> Int -> String -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
mapTerm onVar onType = walk
  where walk c (Var (Just i) n) = onVar c i n
        walk _ (Var Nothing  n) = Var Nothing n
        walk c (Abs n ty t)     = Abs n (onType c ty) (walk (c+1) t)
        walk c (App t1 t2)      = App (walk c t1) (walk c t2)
        walk c (AbsT n t)       = AbsT n (walk c t)
        walk c (ArgT ty)        = ArgT (onType c ty)
