{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator.Evaluator
  ( eval,
    evalParam,
    run
  )
where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.EvaluationContext
import Evaluator.Helper (Error (..), readValue)
import Model.Model (Application (..), Expression (..), Fonction (..))
import Text.Read (readMaybe)

run :: ExceptT Error IO Int -> IO ()
run main =
  runExceptT main >>= internalRun

internalRun :: Either Error Int -> IO ()
internalRun (Left (Error e)) = putStrLn e
internalRun (Right program) = putStrLn ("Resultat : " ++ show program)

eval :: EvalContext m => Application -> m Int
eval app = evalParam app []

evalParam :: EvalContext m => Application -> [Int] -> m Int
evalParam (Application mainFunc otherFunc) =
  runFonction mainFunc (GlobalContext otherFunc 0)

runFonction :: EvalContext m => Fonction -> GlobalContext -> [Int] -> m Int
runFonction (Fonction (first :| others) paramCount) context funcParam
  | Prelude.length funcParam /= paramCount = throwError $ Error "Invalid number of call arguments"
  | otherwise =
    let preFuncContext = PreFunctionContext funcParam context
     in do
          firstValue <- evalExpressionWithPreContext first preFuncContext
          evalExpressions others (initEvaluationContext firstValue preFuncContext)

type EvalContext m = (MonadError Error m, MonadIO m)

evalExpressions :: EvalContext m => [Expression] -> FunctionContext -> m Int
evalExpressions (current : nexts) context =
  do
    val <- evalExpressionInContext context current
    evalExpressions nexts val
evalExpressions [] (FunctionContext _ (final :| other) _) = return final

evalExpressionInContext :: EvalContext m => FunctionContext -> Expression -> m FunctionContext
evalExpressionInContext context expr =
  do
    newVal <- evalExpression expr context
    return $ evolveContext context newVal

evalExpression :: EvalContext m => Expression -> FunctionContext -> m Int
evalExpression (AdditionExpression first second) context =
  do
    param1 <- readContext context first
    param2 <- readContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context =
  let expressionSelector x = if x == 0 then nullExpression else notNullExpression
   in readContext context cond >>= (\x -> evalExpression (expressionSelector x) context)
--
evalExpression (FuncCall funcIndex params) context =
  do
    func <- readFuncInContext'' context funcIndex
    param <- mapM (readContext context) params
    runFonction func (globalContext $ initContext context) param
--
evalExpression expr context =
  evalExpressionWithPreContext expr (initContext context)

evalExpressionWithPreContext :: EvalContext m => Expression -> PreFunctionContext -> m Int
evalExpressionWithPreContext InvalidExpression _ = throwError (Error "Invalid Expression")
--
evalExpressionWithPreContext (ConstExpression val) _ = return val
--
evalExpressionWithPreContext (ParamExpression paramIndex) context =
  readParamInContext context paramIndex
--
evalExpressionWithPreContext (FuncCall funcIndex []) context =
  readFuncInContext' context funcIndex >>= (\x -> runFonction x (globalContext context) [])
--
evalExpressionWithPreContext (ExternalExpression _ _) _ = liftIO readValue
--
evalExpressionWithPreContext _ _ =
  throwError (Error "Invalid index expression")
