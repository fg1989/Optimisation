{-# LANGUAGE FlexibleContexts #-}

module Evaluator.Helper where

import Control.Monad.Except (MonadError (throwError))

newtype Error = Error String deriving (Show, Eq)

readValue :: IO Int
readValue =
  do
    putStrLn "Entrez un nombre :"
    line <- getLine
    _readValue (readMaybe $ toString line)

_readValue :: Maybe Int -> IO Int
_readValue Nothing = readValue
_readValue (Just val) = return val

safeRead :: MonadError Error m => [a] -> Word -> Error -> m a
safeRead list index errorMessage =
  let result = safeRead' list index
   in case result of
        Nothing -> throwError errorMessage
        Just a -> return a

safeRead' :: [a] -> Word -> Maybe a
safeRead' [] _ = Nothing
safeRead' (first : _) 0 = Just first
safeRead' (_ : others) index = safeRead' others $ index - 1
