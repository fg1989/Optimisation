{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Model where

import Data.List.NonEmpty

newtype FonctionIndex = FonctionIndex {funcIndex :: Int} deriving (Show, Eq, Ord, Num)

newtype ExpressionIndex = ExpressionIndex {exprIndex :: Int} deriving (Show, Eq, Ord, Num)

newtype ParamIndex = ParamIndex {paramIndex :: Int} deriving (Show, Eq, Ord, Num)

data ExternalType
  = SimpleExternal
  | UniqueExternal
  | OrderedExternal
  deriving (Show)

data Expression
  = ConstExpression Int
  | AdditionExpression ExpressionIndex ExpressionIndex
  | ConditionalExpression ExpressionIndex Expression Expression
  | InvalidExpression
  | ParamExpression ParamIndex
  | ExternalExpression ExternalType ExpressionIndex
  | -- Comment valider que le nombre de parametre de la fonction est egal aux nombres de parametres passés ?
    FuncCall FonctionIndex [ExpressionIndex]
  deriving (Show)

data Fonction = Fonction (NonEmpty Expression) Int deriving (Show)

data Application = Application Fonction [Fonction] deriving (Show)
