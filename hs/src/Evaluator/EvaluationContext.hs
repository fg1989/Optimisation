{-# LANGUAGE FlexibleContexts #-}

module Evaluator.EvaluationContext where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.Helper (Error (..), safeRead, MaybeError)
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

class (ParamContext ctx) => EvaluationContext ctx where
  index :: ctx -> ExpressionIndex
  stack :: ctx -> NonEmpty Int
  evolveContext :: ctx -> Int -> ctx

-- Le fait que chaque contexte contienne un autre contexte est un détail d'implémentation,
-- il ne devrait pas apparaitre dans l'interface
class EvaluationContext' ctx where
  initEvaluationContext :: ParamContext c => Int -> c -> ctx c

data GlobalContext = GlobalContext [Fonction] Int deriving (Show)

data CallContext g = CallContext [Int] g deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data RunContext c = RunContext ExpressionIndex (NonEmpty Int) c deriving (Show)

instance FonctionContext GlobalContext where
  fonctions (GlobalContext func _) = func
  source (GlobalContext _ src) = src

instance (FonctionContext g) => FonctionContext (CallContext g) where
  fonctions (CallContext _ ctx) = fonctions ctx
  source (CallContext _ ctx) = source ctx

instance (FonctionContext g) => FonctionContext (RunContext g) where
  fonctions (RunContext _ _ ctx) = fonctions ctx
  source (RunContext _ _ ctx) = source ctx

instance (FonctionContext g) => ParamContext (CallContext g) where
  param (CallContext para _) = para

instance (ParamContext c) => ParamContext (RunContext c) where
  param (RunContext _ _ ctx) = param ctx

instance (ParamContext c) => EvaluationContext (RunContext c) where
  index (RunContext i _ _) = i
  stack (RunContext _ s _) = s
  evolveContext (RunContext index stack ctx) val = RunContext (index + 1) (val :| toList stack) ctx

instance EvaluationContext' RunContext where
  initEvaluationContext val = RunContext 0 $ val :| []

readContext :: (MaybeError m, EvaluationContext g) => g -> ExpressionIndex -> m Int
readContext context readIndex =
  safeRead (toList $ stack context) (exprIndex $ index context - readIndex) (Error "Invalid index expression")

readFuncInContext :: (MaybeError m, FonctionContext c) => c -> FonctionIndex -> m Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index (Error "Invalid func index")

readParamInContext :: (MaybeError m, ParamContext c) => c -> ParamIndex -> m Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index (Error "Invalid param index")
