module Main where

import Evaluator.Evaluator (evalParam, run)
import Model.Model
  ( Application (..),
    Expression (..),
    ExternalType (..),
    Fonction (..),
    InstructionList (..),
    SimpleExpression (..),
  )

sommeSimple :: Application
sommeSimple =
  Application
    ( Fonction
        (InstructionList (ExternalExpression SimpleExternal 0) [FuncCall 0 (0 :| [])])
        1
    )
    [ Fonction
        ( InstructionList
            (ParamExpression 0)
            [ Simple $ ConstExpression (-1),
              AdditionExpression 0 1,
              ConditionalExpression 2 (FuncCall 0 $ 2 :| []) (Simple $ ConstExpression 0),
              AdditionExpression 0 3
            ]
        )
        1
    ]

main :: IO ()
main = run $ evalParam sommeSimple [5]
