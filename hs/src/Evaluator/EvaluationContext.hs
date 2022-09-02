module Evaluator.EvaluationContext where

import Data.List.NonEmpty
import Model.Model

newtype Error = Error String deriving (Show)

data GlobalContext = GlobalContext {fonctions :: [Fonction], source :: Int}

data PreFunctionContext = PreFunctionContext {param :: [Int], globalContext :: GlobalContext}

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data FunctionContext = FunctionContext {index :: ExpressionIndex, stack :: NonEmpty Int, initContext :: PreFunctionContext}

initEvaluationContext :: Int -> PreFunctionContext -> FunctionContext
initEvaluationContext val = FunctionContext 0 (val :| [])

-- Existe il une meilleure manière de faire évoluer le contexte (éviter le toList) ?
evolveContext :: FunctionContext -> Int -> FunctionContext
evolveContext context@(FunctionContext currentIndex currentStack _) val =
  context {index = currentIndex + 1, stack = val :| toList currentStack}

readContext :: FunctionContext -> ExpressionIndex -> Either Error Int
readContext context pos
  | index context < pos = Left $ Error "Invalid index expression"
  | otherwise = Right $ stack context Data.List.NonEmpty.!! exprIndex (index context - pos)

readFuncInContext :: GlobalContext -> FonctionIndex -> Either Error Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions context) index $ Error "Invalid func index"

-- Peut on éviter la duplication
readFuncInContext' :: PreFunctionContext -> FonctionIndex -> Either Error Fonction
readFuncInContext' = readFuncInContext . globalContext

-- Peut on éviter la duplication
readFuncInContext'' :: FunctionContext -> FonctionIndex -> Either Error Fonction
readFuncInContext'' = readFuncInContext' . initContext

readParamInContext :: PreFunctionContext -> ParamIndex -> Either Error Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index $ Error "Invalid param index"

-- Peut on éviter la duplication
readParamInContext' :: FunctionContext -> ParamIndex -> Either Error Int
readParamInContext' = readParamInContext . initContext

safeRead :: [a] -> Int -> Error -> Either Error a
safeRead list pos errorMessage
  | Prelude.length list <= pos = Left errorMessage
  | otherwise = Right $ list Prelude.!! pos
