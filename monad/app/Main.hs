module Main where

import qualified MyLib (someFunc)

import Evaluator
import MyState
import MyType
import Parser


main :: IO ()
main = do
--- BASE
--  print $ evalBase answerExpr
--  print $ evalBase errorExpr

--- Ex
--  print $ evalExBase answerExpr
--  print $ evalExBase errorExpr

--- State
--  print $ evalStateBase answerExpr 0

--- Output
--  print $ evalOutputBase answerExpr

--- MonadEX
--  print $ evalME answerExpr
--  print $ evalME errorExpr

--- MonadState
--  print $ runState (evalMS answerExpr) 0

--- MonadWriter
--  print $ evalMW answerExpr

  let res = getParsedResult termP (fromSList "(15/(6/2))")
  case res of
    MyNothing -> print "error parsing"
    MyJust a  -> do
      print a
      print (evalME a)