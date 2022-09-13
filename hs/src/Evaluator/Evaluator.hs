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
    ( EvaluationContext'',
      FonctionContext'(..),
      ProgramContext'(..),
      EvaluationContext'(..),
      EvaluationContext(..),
      FonctionContext(..),
      ProgramContext(..),
      FonctionContext'',
      ProgramContext'' )
import Evaluator.Helper (Error (..), readValue, MaybeError)
import Model.Model (Application (..), Expression (..), Fonction (..), InstructionList (..), SimpleExpression (..))
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
  runFonction mainFunc (tmpInit3 otherFunc 0)

-- Comment supprimer cette fonction
tmpInit1 :: (FonctionContext c) => Int -> c -> EvaluationContext'' c
tmpInit1 = initEvaluationContext

-- Comment supprimer cette fonction
tmpInit2 :: (ProgramContext  c )=> [Int]-> c -> FonctionContext'' c
tmpInit2 = initFonctionContext

-- Comment supprimer cette fonction
tmpInit3 :: [Fonction] -> Int -> ProgramContext''
tmpInit3 = initProgramContext

runFonction :: (EvalContext m, ProgramContext c) => Fonction -> c -> [Int] -> m Int
runFonction (Fonction (InstructionList first others) paramCount) context funcParam
  | Prelude.length funcParam /= paramCount = throwError $ Error "Invalid number of call arguments"
  | otherwise =
    let preFuncContext = tmpInit2 funcParam context
     in do
          firstValue <- evalExpressionWithPreContext first preFuncContext
          let tmp = tmpInit1 firstValue preFuncContext
          evalExpressions others tmp

type EvalContext m = (MaybeError m, MonadIO m)

evalExpressions :: (EvalContext m, EvaluationContext c) => [Expression] -> c -> m Int
evalExpressions (current : nexts) context =
  do
    val <- evalExpressionInContext context current
    evalExpressions nexts val
evalExpressions [] context = return $ getFinalValue context

evalExpressionInContext :: (EvalContext m, EvaluationContext c) => c -> Expression -> m c
evalExpressionInContext context expr =
  do
    newVal <- evalExpression expr context
    return $ evolveContext context newVal

evalExpression :: (EvalContext m, EvaluationContext c) => Expression -> c -> m Int
evalExpression (AdditionExpression first second) context =
  do
    param1 <- readValueFromContext context first
    param2 <- readValueFromContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context =
  let expressionSelector x = if x == 0 then nullExpression else notNullExpression
   in readValueFromContext context cond >>= (\x -> evalExpression (expressionSelector x) context)
--
evalExpression (FuncCall funcIndex params) context =
  do
    func <- readFuncFromContext context funcIndex
    param <- mapM (readValueFromContext context) params
    runFonction func context (toList param)
--
evalExpression (Simple expr) context =
  evalExpressionWithPreContext expr context

evalExpressionWithPreContext :: (EvalContext m, FonctionContext c) => SimpleExpression -> c -> m Int
evalExpressionWithPreContext InvalidExpression _ = throwError (Error "Invalid Expression")
--
evalExpressionWithPreContext (ConstExpression val) _ = return val
--
evalExpressionWithPreContext (ParamExpression paramIndex) context =
  readParamFromContext context paramIndex
--
evalExpressionWithPreContext (SimpleCall funcIndex) context =
  readFuncFromContext context funcIndex >>= (\x -> runFonction x context [])
--
evalExpressionWithPreContext (ExternalExpression _ _) _ = liftIO readValue
