module Evaluator.EvaluationContext where

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

-- Existe il une meilleur manière d'implémenter cette méthode (éviter le toList et le lenght) ?
readContext :: FunctionContext -> ExpressionIndex -> Either Error Int
readContext context (ExpressionIndex index) =
  safeRead (toList $ stack context) (fromIntegral (length (stack context)) - index - 1) (Error "Invalid index expression")

readFuncInContext :: GlobalContext -> FonctionIndex -> Either Error Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index (Error "Invalid func index")

-- Peut on éviter la duplication
readFuncInContext' :: PreFunctionContext -> FonctionIndex -> Either Error Fonction
readFuncInContext' = readFuncInContext . globalContext

-- Peut on éviter la duplication
readFuncInContext'' :: FunctionContext -> FonctionIndex -> Either Error Fonction
readFuncInContext'' = readFuncInContext' . initContext

readParamInContext :: PreFunctionContext -> ParamIndex -> Either Error Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index (Error "Invalid param index")

-- Peut on éviter la duplication
readParamInContext' :: FunctionContext -> ParamIndex -> Either Error Int
readParamInContext' = readParamInContext . initContext
