module MyException where

import Control.Monad
import MyControl 

data MEx a = RaiseEx ExceptionType | ReturnEx a
    deriving Show

type ExceptionType = String

instance MyFunctor MEx where
  myFmap f (RaiseEx s) = RaiseEx s
  myFmap f (ReturnEx a) = ReturnEx (f a)

instance MyApplicative MEx where
  myPure = ReturnEx

  myApply (ReturnEx f) a = myFmap f a
  myApply (RaiseEx s)  _ = RaiseEx s

instance MyMonad MEx where
  myReturn = myPure

  myBind (RaiseEx e) _ = RaiseEx e
  myBind (ReturnEx a) f = f a

raise :: ExceptionType -> MEx a
raise e = RaiseEx e