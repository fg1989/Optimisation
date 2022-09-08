{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator.Evaluator
  ( eval,
    evalParam,
    run,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.EvaluationContext
import Evaluator.Helper (Error (..), readValue)
import Model.Model (Application (..), Expression (..), Fonction (..))
import Text.Read (readMaybe)

newtype Program = Program {mainAction :: ExceptT Error IO Int}

run :: Program -> IO ()
run (Program main) =
  runExceptT main >>= internalRun

internalRun :: Either Error Int -> IO ()
internalRun (Left (Error e)) = putStrLn e
internalRun (Right program) = putStrLn ("Resultat : " ++ show program)

eval :: Application -> Program
eval app = evalParam app []

evalParam :: Application -> [Int] -> Program
evalParam (Application mainFunc otherFunc) params = Program $ runFonction mainFunc (GlobalContext otherFunc 0) params

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

liftEither ::  EvalContext m => Either Error n -> m n
liftEither = either throwError return

evalExpression :: EvalContext m => Expression -> FunctionContext -> m Int
evalExpression (AdditionExpression first second) context =
  liftEither $ do
    param1 <- readContext context first
    param2 <- readContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context =
  let expressionSelector x = if x == 0 then nullExpression else notNullExpression
   in liftEither (readContext context cond) >>= (\x -> evalExpression (expressionSelector x) context)
--
evalExpression (FuncCall funcIndex params) context =
  do
    func <- liftEither $ readFuncInContext'' context funcIndex
    param <- liftEither $ mapM (readContext context) params
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
  liftEither $ readParamInContext context paramIndex
--
evalExpressionWithPreContext (FuncCall funcIndex []) context =
  liftEither (readFuncInContext' context funcIndex) >>= (\x -> runFonction x (globalContext context) [])
--
evalExpressionWithPreContext (ExternalExpression _ _) _ = liftIO readValue
--
evalExpressionWithPreContext _ _ =
  throwError (Error "Invalid index expression")
