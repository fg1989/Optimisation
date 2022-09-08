{-# LANGUAGE FlexibleContexts #-}

module Evaluator.EvaluationContext where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.Helper (Error (..), safeRead)
import Model.Model
  ( ExpressionIndex (..),
    Fonction,
    FonctionIndex (..),
    ParamIndex (..),
  )

-- TODO : implementer source

-- Possibilitée de faire plus simple ?
class FonctionContext ctx where
  fonctions :: ctx -> [Fonction]
  source :: ctx -> Int

class ParamContext ctx where
  param :: ctx -> [Int]

data GlobalContext = GlobalContext [Fonction] Int deriving (Show)

data PreFunctionContext = PreFunctionContext [Int] GlobalContext deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data FunctionContext = FunctionContext {index :: ExpressionIndex, stack :: NonEmpty Int, initContext :: PreFunctionContext} deriving (Show)

instance FonctionContext GlobalContext where
  fonctions (GlobalContext func _) = func
  source (GlobalContext _ src) = src

instance FonctionContext PreFunctionContext where
  fonctions (PreFunctionContext _ ctx) = fonctions ctx
  source (PreFunctionContext _ ctx) = source ctx

instance FonctionContext FunctionContext where
  fonctions (FunctionContext _ _ ctx) = fonctions ctx
  source (FunctionContext _ _ ctx) = source ctx

instance ParamContext PreFunctionContext where
  param (PreFunctionContext para _) = para

initEvaluationContext :: Int -> PreFunctionContext -> FunctionContext
initEvaluationContext val = FunctionContext 0 $ val :| []

-- Existe il une meilleure manière de faire évoluer le contexte (éviter le toList) ?
evolveContext :: FunctionContext -> Int -> FunctionContext
evolveContext context@(FunctionContext currentIndex currentStack _) val =
  context {index = currentIndex + 1, stack = val :| toList currentStack}

-- Existe il une meilleur manière d'implémenter cette méthode (éviter le toList et le lenght) ?
readContext :: MonadError Error m => FunctionContext -> ExpressionIndex -> m Int
readContext context (ExpressionIndex index) =
  safeRead (toList $ stack context) (fromIntegral (length (stack context)) - index - 1) (Error "Invalid index expression")

-- Est ce que changer l'ordre des deux contraintes change quelque chose ?
readFuncInContext :: MonadError Error m => FonctionContext c => c -> FonctionIndex -> m Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index (Error "Invalid func index")

readParamInContext :: MonadError Error m => ParamContext c => c -> ParamIndex -> m Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index (Error "Invalid param index")
