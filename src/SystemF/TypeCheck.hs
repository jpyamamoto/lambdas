module SystemF.TypeCheck ( typeOf
                         , Context
                         , shift
                         , shiftAbove
                         , substTopType
                         , substitute
                         , mapType
                         ) where

import Error
import SystemF.Syntax

type Context = [(String, Type)]

typeOf :: Context -> Term -> Either Error Type
typeOf ctx (Var (Just i) n) = typeFromContext ctx n i
typeOf ctx (Var Nothing  n) = typeFromContext ctx n 0
typeOf ctx (Abs v t b) = do
  tya <- normalize ctx t
  tyb <- typeOf ((v, tya):ctx) b
  return $ Arrow tya tyb
typeOf ctx (App f x)   = do
  tyf <- typeOf ctx f
  tyx <- typeOf ctx x
  case tyf of
    (Arrow tya tyr) -> if tya == tyx
      then Right tyr
      else typeErr $ show x ++ " of type " ++ show tyx ++ " cannot be applied to type " ++ show tyf ++ " of " ++ show f
    (All _ b)       -> return $ substTopType b tyx
    _               -> typeErr $ show tyf ++ " cannot be instanciated with type " ++ show x
typeOf ctx (AbsT v b) = do
  tyb <- typeOf ctx b
  return $ All v tyb
typeOf ctx (ArgT t) = normalize ctx t

typeErr :: String -> Either Error Type
typeErr = Left . TypeError

typeFromContext :: Context -> String -> Int -> Either Error Type
typeFromContext []  name _ = typeErr $ name ++ " has an invalid type"
typeFromContext ctx name 0 = do
  ty <- case lookup name ctx of
    (Just ty) -> Right ty
    Nothing   -> typeErr $ name ++ " has an invalid type"

  normalize ctx ty
typeFromContext ctx name i = do
  ty <- typeFromContext (drop i ctx) name 0
  normalize ctx ty

normalize :: Context -> Type -> Either Error Type
normalize _   (VarT (Just i) n) = Right $ VarT (Just i) n
normalize []  ty = typeErr $ show ty ++ " type does not exist"
normalize ctx (VarT Nothing n) = do
  t <- case lookup n ctx of
    (Just ty) -> Right ty
    Nothing   -> typeErr $ show n ++ " does not exist"

  normalize ctx t
normalize ctx (Arrow t1 t2)     = do
  t1' <- normalize ctx t1
  t2' <- normalize ctx t2
  return $ Arrow t1' t2'
normalize ctx (All n t)         = do
  t' <- normalize ctx t
  return $ All n t'

shift :: Int -> Type -> Type
shift off = shiftAbove off 0

substTopType :: Type -> Type -> Type
substTopType t x = shift (-1) (substitute t 0 (shift 1 x))

mapType :: (Int -> Int -> String -> Type) -> Int -> Type -> Type
mapType onVar = walk
  where walk c (VarT (Just i) n) = onVar c i n
        walk _ (VarT Nothing  n) = VarT Nothing n
        walk c (Arrow t1 t2)     = Arrow (walk c t1) (walk c t2)
        walk c (All n t)         = All n (walk (c + 1) t)

shiftAbove :: Int -> Int -> Type -> Type
shiftAbove off = mapType onVar
  where onVar cut i n
          | i < cut   = VarT (Just i) n
          | otherwise = VarT (Just (i + off)) n

substitute :: Type -> Int -> Type -> Type
substitute ty1 index ty2 = mapType onVar index ty1
  where onVar i j n
          | i == j = shift j ty2
          | otherwise  = VarT (Just i) n
