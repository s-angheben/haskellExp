{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MyData where

import MyType
import GHC.Float (stgWord32ToFloat)

class MySemigroup a where
    mult :: a -> a -> a

---- LAWS for MySemigroup
-- [Associativity ] mult x (mult y z) = mult (mult x y) z

class MySemigroup a => MyMonoid a where
    myMempty :: a

    myMappend :: a -> a -> a
    myMappend = mult


myMconcat :: (Foldable t, MyMonoid a) => t a -> a
myMconcat = foldr myMappend myMempty 

---- LAWS for MyMonoid
-- [Right identity]  myMappend x myMempty = x
-- [Left identity ]  myMappend myMempty x = x
-- [Associativity ]  myMappend x (myMappend y z) = myMappend (myMappend x y) z ('Semigroup' law)
-- [Concatenation ]  @'mconcat' = 'foldr' ('<>') 'mempty'@



-------------------------------------------------
--- INSTANCES FOR MyList

instance MySemigroup (MyList a) where
    mult :: MyList a -> MyList a -> MyList a
    mult = coll 

instance MyMonoid (MyList a) where
    myMempty :: MyList a
    myMempty = Nil

-------------------------------------------------
--- INSTANCES FOR MyMaybe

instance MySemigroup a => MySemigroup (MyMaybe a) where
    mult :: MyMaybe a -> MyMaybe a -> MyMaybe a
    mult MyNothing a = MyNothing 
    mult a MyNothing = MyNothing 
    mult (MyJust a) (MyJust b) = MyJust (mult a b)

instance MySemigroup a => MyMonoid (MyMaybe a) where
    myMempty :: MyMaybe a
    myMempty = MyNothing

-------------------------------------------------
--- INSTANCES FOR String
instance MySemigroup String where
    mult s1 s2 = s1 ++ s2

instance MyMonoid String where
    myMempty = ""


    