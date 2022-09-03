{-# LANGUAGE TupleSections #-}

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

newtype Program = Program {action :: IO (Either Error Int)}

run :: Program -> IO ()
run program = do
  execution <- action program
  internalRun execution

internalRun :: Either Error Int -> IO ()
internalRun (Left (Error e)) = putStrLn e
internalRun (Right program) = do
  putStrLn ("Resultat : " ++ show program)

eval :: Application -> Program
eval app = evalParam app []

evalParam :: Application -> [Int] -> Program
evalParam (Application mainFunc otherFunc) = runFonction mainFunc (GlobalContext otherFunc 0)

runFonction :: Fonction -> GlobalContext -> [Int] -> Program
runFonction (Fonction (first :| others) paramCount) context funcParam
  | Prelude.length funcParam /= paramCount = Program $ return (Left $ Error "Invalid number of call arguments")
  | otherwise =
    let preFuncContext = PreFunctionContext funcParam context
     in let firstValue = evalExpressionWithPreContext first preFuncContext
         in Program $ do
              realFirstValue <- firstValue
              joker realFirstValue (\x -> evalExpressions others (initEvaluationContext x preFuncContext))

evalExpressions :: [Expression] -> FunctionContext -> IO (Either Error Int)
evalExpressions (current : nexts) context =
  do
    val <- evalExpressionInContext context current
    joker val (evalExpressions nexts)
evalExpressions [] (FunctionContext _ (final :| other) _) = return $ Right final

evalExpressionInContext :: FunctionContext -> Expression -> IO (Either Error FunctionContext)
evalExpressionInContext context expr =
  do
    newVal <- evalExpression expr context
    return $ evolveContext context <$> newVal

evalExpression :: Expression -> FunctionContext -> IO (Either Error Int)
evalExpression (AdditionExpression first second) context =
  return $ do
    param1 <- readContext context first
    param2 <- readContext context second
    return $ param1 + param2
--
evalExpression (ConditionalExpression cond notNullExpression nullExpression) context =
  let expressionSelector x = if x == 0 then nullExpression else notNullExpression
   in joker (readContext context cond) (\x -> evalExpression (expressionSelector x) context)
--
evalExpression (FuncCall funcIndex params) context =
  let func = readFuncInContext'' context funcIndex
   in let param = mapM (readContext context) params
       in let val = (func >>= (\x -> (x,) <$> param))
           in joker val (\x -> action $ runFonction (fst x) (globalContext $ initContext context) (snd x))
--
evalExpression expr context =
  evalExpressionWithPreContext expr (initContext context)

evalExpressionWithPreContext :: Expression -> PreFunctionContext -> IO (Either Error Int)
evalExpressionWithPreContext InvalidExpression _ = return $ Left (Error "Invalid Expression")
--
evalExpressionWithPreContext (ConstExpression val) _ = return $ Right val
--
evalExpressionWithPreContext (ParamExpression paramIndex) context =
  return $ readParamInContext context paramIndex
--
evalExpressionWithPreContext (FuncCall funcIndex []) context =
  action $ joker2 (readFuncInContext' context funcIndex) (\x -> runFonction x (globalContext context) [])
--
evalExpressionWithPreContext (ExternalExpression _ _) _ = Right <$> readValue
--
evalExpressionWithPreContext _ _ =
  return $ Left (Error "Invalid index expression")

readValue :: IO Int
readValue =
  do
    putStrLn "Entrez un nombre :"
    line <- getLine
    _readValue (readMaybe line)

_readValue :: Maybe Int -> IO Int
_readValue Nothing = readValue
_readValue (Just val) = return val

joker :: Either Error a -> (a -> IO (Either Error b)) -> IO (Either Error b)
joker (Left err) _ = return $ Left err
joker (Right val) func = func val

joker2 :: Either Error a -> (a -> Program) -> Program
joker2 (Left err) _ = Program (return $ Left err)
joker2 (Right val) func = func val