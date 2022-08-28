module Evaluator.Evaluator
  ( run,
    runParam,
  )
where

import Evaluator.EvaluationContext
import Model.Model

run :: Application -> Either Error Int
run app = runParam app []

runParam :: Application -> [Int] -> Either Error Int
runParam (Application mainFunc otherFunc) =
  runFonction mainFunc (GlobalContext otherFunc 0)

-- est ce que c'est optimisé de continuer a parcourir la liste si on s'arrete
runFonction :: Fonction -> GlobalContext -> [Int] -> Either Error Int
runFonction (Fonction exprs paramCount) context funcParam
  | length funcParam /= paramCount = Left $ Error "Invalid number of call arguments"
  | otherwise =
    head . stack
      <$> foldl
        evalExpressionInContext
        (initEvaluationContext funcParam context)
        exprs

evalExpressionInContext :: Either Error FunctionContext -> Expression -> Either Error FunctionContext
evalExpressionInContext context expr =
  do
    contextValue <- context
    expressionValue <- evalExpression expr contextValue
    return $ evolveContext contextValue expressionValue

evalExpression :: Expression -> FunctionContext -> Either Error Int
evalExpression InvalidExpression _ = Left $ Error "Invalid Expression"
--
evalExpression (ConstExpression val) _ = Right val
--
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
evalExpression (ParamExpression paramIndex) context =
  readParamInContext context paramIndex
--
evalExpression (FuncCall funcIndex params) context =
  do
    func <- readFuncInContext context funcIndex
    paramValue <- mapM (readContext context) params
    runFonction func (globalContext context) paramValue
--
evalExpression (ExternalExpression _ _) _ =
  undefined
