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

class (FonctionContext ctx) => ParamContext ctx where
  param :: ctx -> [Int]

data GlobalContext = GlobalContext [Fonction] Int deriving (Show)

data CallContext g = CallContext [Int] g deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data EvaluationContext c = EvaluationContext {index :: ExpressionIndex, stack :: NonEmpty Int, initContext :: c} deriving (Show)

instance FonctionContext GlobalContext where
  fonctions (GlobalContext func _) = func
  source (GlobalContext _ src) = src

instance (FonctionContext g) => FonctionContext (CallContext g) where
  fonctions (CallContext _ ctx) = fonctions ctx
  source (CallContext _ ctx) = source ctx

instance (FonctionContext g) => FonctionContext (EvaluationContext g) where
  fonctions (EvaluationContext _ _ ctx) = fonctions ctx
  source (EvaluationContext _ _ ctx) = source ctx

instance (FonctionContext g) => ParamContext (CallContext g) where
  param (CallContext para _) = para

initEvaluationContext :: Int -> CallContext g -> EvaluationContext (CallContext g)
initEvaluationContext val = EvaluationContext 0 $ val :| []

-- Existe il une meilleure manière de faire évoluer le contexte (éviter le toList) ?
evolveContext :: EvaluationContext g -> Int -> EvaluationContext g
evolveContext (EvaluationContext currentIndex currentStack context) val =
  EvaluationContext (currentIndex + 1) (val :| toList currentStack) context

-- Existe il une meilleur manière d'implémenter cette méthode (éviter le toList et le lenght) ?
readContext :: MonadError Error m => EvaluationContext g -> ExpressionIndex -> m Int
readContext context (ExpressionIndex index) =
  safeRead (toList $ stack context) (fromIntegral (length (stack context)) - index - 1) (Error "Invalid index expression")

readFuncInContext :: (MonadError Error m, FonctionContext c) => c -> FonctionIndex -> m Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index (Error "Invalid func index")

readParamInContext :: (MonadError Error m, ParamContext c) => c -> ParamIndex -> m Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index (Error "Invalid param index")
