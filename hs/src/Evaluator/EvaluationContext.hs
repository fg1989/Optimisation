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
data GlobalContext = GlobalContext {fonctions :: [Fonction], source :: Int} deriving (Show)

data PreFunctionContext = PreFunctionContext {param :: [Int], globalContext :: GlobalContext} deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data FunctionContext = FunctionContext {index :: ExpressionIndex, stack :: NonEmpty Int, initContext :: PreFunctionContext} deriving (Show)

initEvaluationContext :: Int -> PreFunctionContext -> FunctionContext
initEvaluationContext val = FunctionContext 0 $ val :| []

-- Existe il une meilleure manière de faire évoluer le contexte (éviter le toList) ?
evolveContext :: FunctionContext -> Int -> FunctionContext
evolveContext context@(FunctionContext currentIndex currentStack _) val =
  context {index = currentIndex + 1, stack = val :| toList currentStack}

-- Existe il une meilleur manière d'implémenter cette méthode (éviter le toList) ?
readContext :: MonadError Error m => FunctionContext -> ExpressionIndex -> m Int
readContext context readIndex =
  safeRead (toList $ stack context) (exprIndex $ index context - readIndex) (Error "Invalid index expression")

readFuncInContext :: MonadError Error m => GlobalContext -> FonctionIndex -> m Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index (Error "Invalid func index")

-- Peut on éviter la duplication
readFuncInContext' :: MonadError Error m => PreFunctionContext -> FonctionIndex -> m Fonction
readFuncInContext' = readFuncInContext . globalContext

-- Peut on éviter la duplication
readFuncInContext'' :: MonadError Error m => FunctionContext -> FonctionIndex -> m Fonction
readFuncInContext'' = readFuncInContext' . initContext

readParamInContext :: MonadError Error m => PreFunctionContext -> ParamIndex -> m Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index (Error "Invalid param index")

-- Peut on éviter la duplication
readParamInContext' :: MonadError Error m => FunctionContext -> ParamIndex -> m Int
readParamInContext' = readParamInContext . initContext
