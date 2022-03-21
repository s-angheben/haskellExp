{-# LANGUAGE DeriveFoldable #-}

module MyType where

import Data.Foldable

data MyList a = Cons a (MyList a) | Nil
    deriving (Foldable)


toSList :: MyList a -> [a]
toSList Nil = []
toSList (Cons x xs) = x:toSList xs

fromSList :: [a] -> MyList a
fromSList = foldr Cons Nil

instance Show a => Show (MyList a) where
  show Nil = "Nil"
  show (Cons a xs) = show a ++ ";" ++ show xs


coll :: MyList a -> MyList a -> MyList a
coll Nil ys = ys
coll (Cons x xs) ys = Cons x (coll xs ys)


data MyMaybe a = MyJust a | MyNothing
    deriving (Show, Foldable)


data Term = Con Int
          | Div Term Term
    deriving Show