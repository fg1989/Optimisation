{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Evaluator.Evaluator
  ( eval,
    evalParam,
    run,
  )
where

import Data.List.NonEmpty
import Evaluator.EvaluationContext
import Model.Model
import Text.Read (readMaybe)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))

newtype Program = Program {action :: ExceptT Error IO Int}

run :: Program -> IO ()
run program = runExceptT (action program) >>= \case
  Left (Error e) -> putStrLn e
  Right result   -> putStrLn ("Resultat : " <> show result)

eval :: Application -> Program
eval app = evalParam app []

evalParam :: Application -> [Int] -> Program
evalParam (Application mainFunc otherFunc) = runFonction mainFunc (GlobalContext otherFunc 0)

runFonction :: Fonction -> GlobalContext -> [Int] -> Program
runFonction (Fonction (first :| others) paramCount) context funcParam
  | Prelude.length funcParam /= paramCount = Program $ throwError $ Error "Invalid number of call arguments"
  | otherwise = Program $ do
    let preFuncContext = PreFunctionContext funcParam context
    firstValue <- evalExpressionWithPreContext first preFuncContext
    evalExpressions others (initEvaluationContext firstValue  preFuncContext)

runProgramInEval :: EvalContext m => Program -> m Int
runProgramInEval program = do
  res <- liftIO $ runExceptT (action program)
  either throwError pure res

type EvalContext m = (MonadError Error m, MonadIO m)

evalExpressions :: EvalContext m => [Expression] -> FunctionContext -> m Int
evalExpressions (current : nexts) context = do
  val <- evalExpressionInContext context current
  evalExpressions nexts val
evalExpressions [] (FunctionContext _ (final :| other) _) = return final

evalExpressionInContext :: EvalContext m => FunctionContext -> Expression -> m FunctionContext
evalExpressionInContext context expr = do
  newVal <- evalExpression expr context
  return $ evolveContext context newVal

-- | Util function to lift (Either Error) into an EvalContext
liftEither :: EvalContext m => Either Error a -> m a
liftEither = either throwError return

evalExpression :: EvalContext m => Expression -> FunctionContext -> m Int
evalExpression (AdditionExpression first second) context =
  liftEither $ do
    param1 <- readContext context first
    param2 <- readContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context = do
  let expressionSelector 0 = nullExpression
      expressionSelector _ = notNullExpression
  val <- liftEither $ readContext context cond
  evalExpression (expressionSelector val) context
--
evalExpression (FuncCall funcIndex params) context = do
  func <- liftEither $ readFuncInContext'' context funcIndex
  param <- liftEither $ mapM (readContext context) params
  let program = runFonction func (globalContext $ initContext context) param
  runProgramInEval program
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
evalExpressionWithPreContext (FuncCall funcIndex []) context = do
  fn <- liftEither $ readFuncInContext' context funcIndex
  let program = runFonction fn (globalContext context) []
  runProgramInEval program
--
evalExpressionWithPreContext (ExternalExpression _ _) _ = liftIO readValue
--
evalExpressionWithPreContext _ _ =
  throwError (Error "Invalid index expression")

readValue :: IO Int
readValue =
  do
    putStrLn "Entrez un nombre :"
    line <- getLine
    _readValue (readMaybe line)

_readValue :: Maybe Int -> IO Int
_readValue Nothing = readValue
_readValue (Just val) = return val
