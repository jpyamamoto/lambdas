module Untyped.Interpreter ( interpret ) where

import qualified Data.Map as M
import Control.Monad.State.Lazy

import Untyped.Syntax
import Untyped.Evaluation
import Components (Interpreter)
import Error

type Output = Either Error (Maybe Result)

interpret :: Interpreter (Instruction Term) Result
interpret l = sequence $ evalState (mapM runInstruction l) M.empty

runInstruction :: Instruction Term -> State Context Output
runInstruction (Eval p t)  = do
  ctx <- get
  return . Right . Just $ Result (p, eval ctx t)
runInstruction (Defn n t)  = do
  ctx <- get
  put $ M.insert n t ctx
  return $ Right Nothing
runInstruction (Info p t)  = do
  ctx <- get
  return . Right . Just $ Result (p, inspect ctx t)

inspect :: Context -> Term -> Term
inspect ctx (Var Nothing name) = case M.lookup name ctx of
  (Just t) -> t
  Nothing  -> Var Nothing name
inspect _ t = t
