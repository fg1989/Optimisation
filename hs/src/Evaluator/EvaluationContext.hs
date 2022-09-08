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
data GlobalContext = GlobalContext [Fonction] Int deriving (Show)

data PreFunctionContext = PreFunctionContext [Int] GlobalContext deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data FunctionContext = FunctionContext ExpressionIndex (NonEmpty Int) PreFunctionContext deriving (Show)

initEvaluationContext :: Int -> PreFunctionContext -> FunctionContext
initEvaluationContext val = FunctionContext 0 $ val :| []

-- Existe il une meilleure manière de faire évoluer le contexte (éviter le toList) ?
evolveContext :: FunctionContext -> Int -> FunctionContext
evolveContext (FunctionContext currentIndex currentStack context) val =
  FunctionContext (currentIndex + 1) (val :| toList currentStack) context

-- Existe il une meilleur manière d'implémenter cette méthode (éviter le toList et le lenght) ?
readContext :: MonadError Error m => FunctionContext -> ExpressionIndex -> m Int
readContext (FunctionContext _ stack _) (ExpressionIndex index) =
  safeRead (toList stack) (fromIntegral (length stack) - index - 1) (Error "Invalid index expression")

readFuncInContext :: MonadError Error m => GlobalContext -> FonctionIndex -> m Fonction
readFuncInContext (GlobalContext fonctions _) (FonctionIndex index) =
  safeRead fonctions index (Error "Invalid func index")

readParamInContext :: MonadError Error m => PreFunctionContext -> ParamIndex -> m Int
readParamInContext (PreFunctionContext param _) (ParamIndex index) =
  safeRead param index (Error "Invalid param index")
