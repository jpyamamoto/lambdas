module Untyped.Interpreter (interpret) where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import Untyped.Definitions
import Components (Interpreter, Error)

type Context = M.Map String Term
type Output = Either Error (Maybe Result)

interpret :: Interpreter (Instruction Term) Result
interpret l = sequence $ evalState (mapM runInstruction l) M.empty

runInstruction :: Instruction Term -> State Context Output
runInstruction (Eval p t)  = do
  context <- get
  return . Right . Just $ Result (p, eval context t)
runInstruction (Defn n t)  = do
  context <- get
  put $ M.insert n t context
  return $ Right Nothing
runInstruction (Info p t)  = do
  context <- get
  return . Right . Just $ Result (p, inspect context t)

inspect :: Context -> Term -> Term
inspect context (Var Nothing name) = case M.lookup name context of
  (Just t) -> t
  Nothing  -> Var Nothing name
inspect _ t = t

eval :: Context -> Term -> Term
eval ctx t = case beta ctx t of
  (Just t') -> eval ctx t'
  Nothing   -> t

beta :: Context -> Term -> Maybe Term
beta ctx (Var Nothing name) = M.lookup name ctx
beta ctx (App (Abs _ t) x)
  | isVal ctx x = Just $ shift (-1) 0 (substitute t 0 (shift 1 0 x))
  | otherwise   = beta ctx x
beta ctx (App f x) = beta ctx f >>= \t' -> Just (App t' x)
beta _ _ = Nothing

isVal :: Context -> Term -> Bool
isVal ctx (Var Nothing name) = not $ M.member name ctx
isVal _   (Var _ _) = True
isVal _   (Abs _ _) = True
isVal _   _         = False

shift :: Int -> Int -> Term -> Term
shift _      _      (Var Nothing n) = Var Nothing n
shift offset cutoff (Var (Just i) n)
  | i < cutoff = Var (Just i) n
  | otherwise  = Var (Just (i + offset)) n
shift offset cutoff (Abs n t) = Abs n $ shift offset (cutoff + 1) t
shift offset cutoff (App f x) = App (shift offset cutoff f)
                                    (shift offset cutoff x)

substitute :: Term -> Int -> Term -> Term
substitute (Var Nothing n) _ _ = Var Nothing n
substitute (Var (Just i) n) index t
  | i == index = t
  | otherwise  = Var (Just i) n
substitute (Abs n t1) index t2 = Abs n $
                                 substitute t1 (index + 1) (shift 1 0 t2)
substitute (App f x)  index t2 = App (substitute f index t2)
                                     (substitute x index t2)
