{-# LANGUAGE FlexibleContexts #-}

module Evaluator.EvaluationContext where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.Helper (Error (..), MaybeError, safeRead)
import Model.Model
  ( ExpressionIndex (..),
    Fonction,
    FonctionIndex (..),
    ParamIndex (..),
  )

-- TODO : implementer source

-- Possibilitée de faire plus simple ?

-- Le fait que chaque contexte contienne un autre contexte est un détail d'implémentation,
-- il ne devrait pas apparaitre dans l'interface

class ProgramContext ctx where
  readFuncFromContext :: (MaybeError m) => ctx -> FonctionIndex -> m Fonction

class (ProgramContext ctx) => FonctionContext ctx where
  readParamFromContext :: (MaybeError m) => ctx -> ParamIndex -> m Int

class (FonctionContext ctx) => EvaluationContext ctx where
  evolveContext :: ctx -> Int -> ctx
  readValueFromContext :: (MaybeError m) => ctx -> ExpressionIndex -> m Int
  getFinalValue :: ctx -> Int

-- Possibilité de mettre une contrainte (FonctionContext i) => (EvaluationContext (ctx i)) =>
class EvaluationContext' ctx where
  initEvaluationContext :: FonctionContext c => Int -> c -> ctx c

class FonctionContext' ctx where
  initFonctionContext :: ProgramContext c => [Int] -> c -> ctx c

class ProgramContext' ctx where
  initProgramContext :: [Fonction] -> Int -> ctx

data ProgramContext'' = ProgramContext'' [Fonction] Int deriving (Show)

data FonctionContext'' g = FonctionContext'' [Int] g deriving (Show)

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data EvaluationContext'' c = EvaluationContext'' ExpressionIndex (NonEmpty Int) c deriving (Show)

-- Correct d'utiliser ce type de nommage vu que la classe n'a qu'une instance ?
instance ProgramContext ProgramContext'' where
  readFuncFromContext (ProgramContext'' func _) (FonctionIndex index) =
    safeRead func index (Error "Invalid func index")

instance (ProgramContext g) => ProgramContext (FonctionContext'' g) where
  readFuncFromContext (FonctionContext'' _ ctx) = readFuncFromContext ctx

instance (ProgramContext g) => ProgramContext (EvaluationContext'' g) where
  readFuncFromContext (EvaluationContext'' _ _ ctx) = readFuncFromContext ctx

instance (ProgramContext g) => FonctionContext (FonctionContext'' g) where
  readParamFromContext (FonctionContext'' param _) (ParamIndex index) =
    safeRead param index (Error "Invalid param index")

instance (FonctionContext c) => FonctionContext (EvaluationContext'' c) where
  readParamFromContext (EvaluationContext'' _ _ ctx) = readParamFromContext ctx

instance (FonctionContext c) => EvaluationContext (EvaluationContext'' c) where
  evolveContext (EvaluationContext'' index stack ctx) val =
    EvaluationContext'' (index + 1) (val :| toList stack) ctx
  readValueFromContext (EvaluationContext'' index stack _) readIndex =
    safeRead (toList stack) (exprIndex $ index - readIndex) (Error "Invalid index expression")
  getFinalValue (EvaluationContext'' _ stack _) = head stack

instance EvaluationContext' EvaluationContext'' where
  initEvaluationContext val = EvaluationContext'' 0 $ val :| []

instance FonctionContext' FonctionContext'' where
  initFonctionContext = FonctionContext''

instance ProgramContext' ProgramContext'' where
  initProgramContext = ProgramContext''
