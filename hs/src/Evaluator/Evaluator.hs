module Evaluator.Evaluator
  ( run,
    runParam,
  )
where

import Data.List.NonEmpty
import Evaluator.EvaluationContext
import Model.Model

run :: Application -> Either Error Int
run app = runParam app []

runParam :: Application -> [Int] -> Either Error Int
runParam (Application mainFunc otherFunc) =
  runFonction mainFunc (GlobalContext otherFunc 0)

runFonction :: Fonction -> GlobalContext -> [Int] -> Either Error Int
runFonction (Fonction (first :| others) paramCount) context funcParam
  | Prelude.length funcParam /= paramCount = Left $ Error "Invalid number of call arguments"
  | otherwise =
    let preFuncContext = PreFunctionContext funcParam context
     in let firstValue = evalExpressionWithPreContext first preFuncContext
         in do
              realFirstValue <- firstValue
              evalExpressions others (initEvaluationContext realFirstValue preFuncContext)

evalExpressions :: [Expression] -> FunctionContext -> Either Error Int
evalExpressions (current : nexts) context =
  evalExpressionInContext context current >>= evalExpressions nexts
evalExpressions [] (FunctionContext _ (final :| other) _) = Right final

evalExpressionInContext :: FunctionContext -> Expression -> Either Error FunctionContext
evalExpressionInContext context expr =
  evolveContext context <$> evalExpression expr context

evalExpression :: Expression -> FunctionContext -> Either Error Int
evalExpression (AdditionExpression first second) context =
  do
    param1 <- readContext context first
    param2 <- readContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context =
  let expressionSelector x = if x == 0 then nullExpression else notNullExpression
   in readContext context cond >>= \x -> evalExpression (expressionSelector x) context
--
evalExpression (FuncCall funcIndex params) context =
  do
    func <- readFuncInContext'' context funcIndex
    paramValue <- mapM (readContext context) params
    runFonction func (globalContext $ initContext context) paramValue
--
evalExpression expr context =
  evalExpressionWithPreContext expr (initContext context)

evalExpressionWithPreContext :: Expression -> PreFunctionContext -> Either Error Int
evalExpressionWithPreContext InvalidExpression _ = Left $ Error "Invalid Expression"
--
evalExpressionWithPreContext (ConstExpression val) _ = Right val
--
evalExpressionWithPreContext (ParamExpression paramIndex) context =
  readParamInContext context paramIndex
--
evalExpressionWithPreContext (FuncCall funcIndex []) context =
  do
    func <- readFuncInContext' context funcIndex
    runFonction func (globalContext context) []
--
evalExpressionWithPreContext (ExternalExpression _ _) _ =
  undefined -- TODO
  --
evalExpressionWithPreContext _ _ =
  Left $ Error "Invalid index expression"