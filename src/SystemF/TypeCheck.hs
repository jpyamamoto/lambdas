module SystemF.TypeCheck ( typeOf
                         , Context
                         , shift
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
  tyb <- typeOf ((v,t):ctx) b
  return $ Arrow t tyb
typeOf ctx (App f x)   = do
  tyf <- typeOf ctx f
  tyx <- typeOf ctx x
  case tyf of
    (Arrow tya tyr) -> if tya == tyx
      then Right tyr
      else typeErr $ show x ++ " of type " ++ show tyx ++ " cannot be applied to type " ++ show tyf
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
normalize []  ty = typeErr $ show ty ++ " type does not exist"
normalize ctx (VarT Nothing n) = do
  t <- case lookup n ctx of
    (Just ty) -> Right ty
    Nothing   -> typeErr $ show n ++ " does not exist"

  normalize ctx t
normalize _   (VarT (Just i) n) = Right $ VarT (Just i) n
normalize ctx (Arrow t1 t2)     = do
  t1' <- normalize ctx t1
  t2' <- normalize ctx t2
  return $ Arrow t1' t2'
normalize ctx (All n t)         = do
  t' <- normalize ctx t
  return $ All n t'

shiftTopType :: Int -> Type -> Type
shiftTopType i = shift i 0

substTopType :: Type -> Type -> Type
substTopType t x = shiftTopType (-1) (substitute t 0 (shiftTopType 1 x))

mapType :: (Int -> Int -> String -> Type) -> Int -> Type -> Type
mapType onVar cut ty = walk cut ty
  where walk c (Arrow t1 t2)     = Arrow (walk c t1) (walk c t2)
        walk _ (VarT (Just i) n) = onVar cut i n
        walk _ (VarT Nothing  n) = VarT Nothing n
        walk c (All n t)         = All n (walk (c + 1) t)

shift :: Int -> Int -> Type -> Type
shift off = mapType onVar
  where onVar cut i n
          | i < cut   = VarT (Just i) n
          | otherwise = VarT (Just (i + off)) n

substitute :: Type -> Int -> Type -> Type
substitute ty1 index ty2 = mapType onVar index ty1
  where onVar i j n
          | i == j = shiftTopType j ty2
          | otherwise  = VarT (Just i) n
