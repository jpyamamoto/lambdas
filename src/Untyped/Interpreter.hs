module Untyped.Interpreter (interpret) where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import Untyped.Definitions

import Components (Interpreter)

type Context = M.Map String Term

interpret :: Interpreter (Instruction Term) Result
interpret l = evalState (mapM runInstruction l) M.empty

runInstruction :: Instruction Term -> State Context (Maybe Result)
runInstruction (Eval p t) = do
  context <- get
  return . Just . Result $ (p, beta context t)
runInstruction (Defn n t)  = do
  context <- get
  put $ M.insert n t context
  return Nothing
runInstruction (Info p t)  = do
  context <- get
  return . Just . Result $ (p, inspect context t)

inspect :: Context -> Term -> Term
inspect context (Var Nothing name) = case M.lookup name context of
  (Just t) -> t
  Nothing  -> Var Nothing name
inspect _ t = t

beta :: Context -> Term -> Term
beta context   (Var Nothing name) = case M.lookup name context of
  (Just t) -> beta context t
  Nothing  -> Var Nothing name
beta _       v@(Var (Just _) _) = v
beta context   (Abs n t) = Abs n (beta context t)
beta context   (App f x) = betaApp context (beta context f) (beta context x)

betaApp :: Context -> Term -> Term -> Term
betaApp context (Abs _ t) x = beta context unshiftedT
  where shiftedX = shift 1 0 x
        subsT = substitute t 0 shiftedX
        unshiftedT = shift (-1) 0 subsT
betaApp _ f x = App f x

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
