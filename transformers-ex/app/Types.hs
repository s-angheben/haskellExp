{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

data Term = Con Int
          | Div Term Term
    deriving Show


data EvalError = DivideByZero
    deriving Show

-- only for tests
newtype Error = Err String
    deriving Show

type Result = Int

newtype Counter = C Int
    deriving (Show) 

addOneC :: Counter -> Counter
addOneC (C n) = C $ n+1

newtype Log = Log String
    deriving (Show, Semigroup, Monoid)


line :: Term -> String -> Log
line t a = Log $ "eval (" ++ show t ++ ") <= " ++ a ++ "\n"