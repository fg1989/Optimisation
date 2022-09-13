{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Model where

newtype FonctionIndex = FonctionIndex {funcIndex :: Word} deriving (Show, Eq, Ord, Num)

newtype ExpressionIndex = ExpressionIndex {exprIndex :: Word} deriving (Show, Eq, Ord, Num)

newtype ParamIndex = ParamIndex {paramIndex :: Word} deriving (Show, Eq, Ord, Num)

data ExternalType
  = SimpleExternal
  | OrderedExternal
  deriving (Show)

data SimpleExpression
  = ConstExpression Int
  | InvalidExpression
  | ParamExpression ParamIndex
  | ExternalExpression ExternalType ExpressionIndex
  | SimpleCall FonctionIndex
  deriving (Show)

data Expression
  = AdditionExpression ExpressionIndex ExpressionIndex
  | ConditionalExpression ExpressionIndex Expression Expression
  | Simple SimpleExpression
  | -- Comment valider que le nombre de parametre de la fonction est egal aux nombres de parametres passés ?
    FuncCall FonctionIndex (NonEmpty ExpressionIndex)
  deriving (Show)

data InstructionList = InstructionList SimpleExpression [Expression] deriving (Show)

data Fonction = Fonction InstructionList Int deriving (Show)

data Application = Application Fonction [Fonction] deriving (Show)
