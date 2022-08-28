module Main where

import Data.List.NonEmpty
import Evaluator.Evaluator
import Model.Model

sommeSimple :: Application
sommeSimple =
  Application
    ( Fonction
        (ParamExpression 0 :| [FuncCall 0 [0]])
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
main = print (runParam sommeSimple [5])
