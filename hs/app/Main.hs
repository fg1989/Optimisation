module Main where

import Evaluator.Evaluator (evalParam, run)
import Model.Model
  ( Application (..),
    Expression (..),
    ExternalType (..),
    Fonction (..),
  )

sommeSimple :: Application
sommeSimple =
  Application
    ( Fonction
        (ExternalExpression SimpleExternal 0 :| [FuncCall 0 [0]])
        1
    )
    [ Fonction
        ( ParamExpression 0
            :| [ ConstExpression (-1),
                 AdditionExpression 0 1,
                 ConditionalExpression 2 (FuncCall 0 [2]) (ConstExpression 0),
                 AdditionExpression 0 3
               ]
        )
        1
    ]

main :: IO ()
main = run $ evalParam sommeSimple [5]
