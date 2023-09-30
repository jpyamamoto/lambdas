module Typed.Interpreter (interpret) where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Data.Bifunctor (Bifunctor(first, second))

import Components (Interpreter)
import Error
import Typed.Syntax
import qualified Typed.Evaluation as E
import qualified Typed.TypeCheck as T

type TermsContext = E.Context
type TypesContext = T.Context
type Output = Either Error (Maybe Result)

typeOf :: TypesContext -> Term -> Either Error Type
typeOf = T.typeOf

interpret :: Interpreter (Instruction Term) Result
interpret l = sequence $ evalState (mapM runInstruction l) (M.empty, [])

runInstruction :: Instruction Term -> State (TermsContext, TypesContext) Output
runInstruction (Eval p t)  = do
  (terms, types) <- get
  let ty = typeOf types t
  return $ Just . const (ResTerm (p, E.eval terms t)) <$> ty
runInstruction (Defn n t)  = do
  modify $ first (M.insert n t)
  (_, ctx) <- get
  let typeDef = typeOf ctx t

  case typeDef of
    (Left err) -> return $ Left err
    (Right ty) -> do
      modify $ second ((n, ty):)
      return . Right $ Nothing
runInstruction (Info p t)  = do
  (terms, types) <- get
  let ty = typeOf types t
  return $ Just . const (ResTerm (p, inspect terms t)) <$> ty
runInstruction (Type p t)  = do
  context <- get
  let ty = typeOf (snd context) t
  return $ Just . ResType . (,) p <$> ty


inspect :: TermsContext -> Term -> Term
inspect context (Var Nothing name) = case M.lookup name context of
  (Just t) -> t
  Nothing  -> Var Nothing name
inspect _ t = t
