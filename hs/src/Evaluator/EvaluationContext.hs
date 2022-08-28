module Evaluator.EvaluationContext where

import Model.Model

newtype Error = Error String deriving (Show)

data GlobalContext = GlobalContext {fonctions :: [Fonction], source :: Int}

-- structure de donneée plus efficace que la liste pour l'accès indexé (arbre ?) ?
data FunctionContext = EvaluationContext
  { index :: ExpressionIndex,
    stack :: [Int],
    param :: [Int],
    globalContext :: GlobalContext
  }

initEvaluationContext :: [Int] -> GlobalContext -> Either Error FunctionContext
initEvaluationContext funcParam context =
  Right $ EvaluationContext (-1) [] funcParam context

evolveContext :: FunctionContext -> Int -> FunctionContext
evolveContext context val =
  EvaluationContext (index context + 1) (val : stack context) (param context) (globalContext context)

readContext :: FunctionContext -> ExpressionIndex -> Either Error Int
readContext context pos
  | index context >= pos = Right $ stack context !! exprIndex (index context - pos)
  | otherwise = Left $ Error "Invalid index expression"

readFuncInContext :: FunctionContext -> FonctionIndex -> Either Error Fonction
readFuncInContext context (FonctionIndex index) =
  safeRead (fonctions $ globalContext context) index $ Error "Invalid func index"

readParamInContext :: FunctionContext -> ParamIndex -> Either Error Int
readParamInContext context (ParamIndex index) =
  safeRead (param context) index $ Error "Invalid param index"

safeRead :: [a] -> Int -> Error -> Either Error a
safeRead list pos errorMessage
  | length list > pos = Right $ list !! pos
  | otherwise = Left errorMessage
