module SystemF.TypeCheck ( typeOf
                         , Context
                         ) where

import Error
import SystemF.Syntax

type Context = [(String, Type)]

typeOf :: Context -> Term -> Either Error Type
typeOf ctx (Var (Just i) n) = typeFromContext ctx n i
typeOf ctx (Var Nothing  n) = typeFromContext ctx n 0
typeOf ctx (Abs v t b) = do
  tyb <- typeOf ((v,t):ctx) b
  return $ Arrow t (shiftTopType (-1) tyb)
typeOf ctx (App f x)   = do
  tyf <- typeOf ctx f
  tyx <- typeOf ctx x
  case tyf of
    (Arrow tya tyr) -> if tya == tyx
      then Right tyr
      else typeErr $ show x ++ " cannot be applied to type " ++ show tyf
    (All _ b)       -> return $ substTopType b tyx
    _               -> typeErr $ show x ++ " cannot be applied to type " ++ show tyf
typeOf ctx (AbsT v b) = do
  tyb <- typeOf ctx b
  return $ All v tyb
typeOf _   (ArgT t) = return t

typeErr :: String -> Either Error Type
typeErr = Left . TypeError

typeFromContext :: Context -> String -> Int -> Either Error Type
typeFromContext []  name _ = typeErr $ name ++ " has an invalid type"
typeFromContext ctx name 0 = case lookup name ctx of
  (Just ty) -> Right ty
  Nothing   -> typeErr $ name ++ " has an invalid type"
typeFromContext ctx name i = typeFromContext (drop i ctx) name 0

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
