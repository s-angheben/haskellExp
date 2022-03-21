{-# LANGUAGE InstanceSigs #-}

module MyControl where

import MyType
import MyData

class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

---- LAWS for MyFunctor
-- [Identity    ] myFmap id == id
-- [Composition ] myFmap (f . g) == myFmap f . myFmap g


class MyFunctor f => MyApplicative f where
    myPure :: a -> f a

    myApply :: f (a -> b) -> f a -> f b

---- LAWS for MyApplicative
-- [Identity    ] myApply (myPure id) v = v
-- [Composition ] myApply (myApply (myApply (myPure (.) u) v) w = myApply u  (MyApply v w)
-- [Homomorphism] myApply (myPure f) (myPure x) = myPure (f x)
-- [Interchange ] myApply u (myPure y) = myApply (myPure ('$' y)) u

class MyApplicative m => MyMonad m where
    myReturn :: a -> m a

    myBind   :: m a -> (a -> m b) -> m b

    myFish :: (a -> m b) -> (b -> m c) -> (a -> m c)
    myFish f g = \x -> myBind (f x) g

----------------
--SOME FUNCTIONS
----------------


join :: MyMonad m => m (m a) -> m a
join x = myBind x id

lift2 :: MyApplicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a = myApply (myFmap f a)

-----------
--INSTANCES
-----------

-------------------------------------------------
--- INSTANCES FOR MyList

instance MyFunctor MyList where
    myFmap :: (a -> b) -> MyList a -> MyList b
    myFmap f Nil = Nil
    myFmap f (Cons a xs) = Cons (f a) (myFmap f xs)

--- Equational Reasoning for MyFunctor Law on MyList
--
--  MyFmap id = id  
----  MyFmap id Nil
----  = {definition of myFmap}
----  Nil
----  = {definition of id}
----  id Nil
----  
----  MyFmap id (Cons a xs)
----  = {definition of MyFmap}
----  Cons (id a) (myFmap f xs)
----  = {definition of id, recursion}
----  Cons (a) xs
----  = {definition of id}
----  id (Cons a xs)
--
-- MyFmap (f . g) = MyFmap f . MyFmap g 
---- MyFmap (f . g) Nil
---- = {definition of MyFmap}
---- Nil
---- = {definition of MyFmap}
---- MyFmap g Nil
---- = {definition of MyFmap}
---- MyFmap f (MyFmap g Nil)
----
---- MyFmap (f . g) list@(Cons a xs)
---- = {definition of MyFmap}
---- Cons ((f . g) a) (MyFmap (f . g) xs)
---- = {definition of composition} 
---- Cons (f (g a)) (MyFmap (f . g) xs)
---- = {definition of MyFmap}
---- MyFmap f (Cons (g a) (MyFmap g xs))
---- = {definition of MyFmap}
---- MyFmap f (MyFmap g (Cons a xs))
---- = {definition of composition}
---- (MyFmap f . MyFmap g) (Cons a xs)

instance MyApplicative MyList where
    myPure :: a -> MyList a
    myPure a = Cons a Nil

    myApply :: MyList (a -> b) -> MyList a -> MyList b
    myApply fs xs = myMconcat $ myFmap (`myFmap` xs) fs

instance MyMonad MyList where
    myReturn :: a -> MyList a
    myReturn = myPure

    myBind :: MyList a -> (a -> MyList b) -> MyList b
    myBind Nil f = Nil
    myBind xs f  = myMconcat $ myFmap f xs


-------------------------------------------------
--- INSTANCES FOR MyMaybe

instance MyFunctor MyMaybe where
  myFmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  myFmap f MyNothing  = MyNothing
  myFmap f (MyJust a) = MyJust (f a)

instance MyApplicative MyMaybe where
    myPure :: a -> MyMaybe a
    myPure = MyJust

    myApply :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
    myApply (MyJust f) a = myFmap f a
    myApply MyNothing  _ = MyNothing

instance MyMonad MyMaybe where
    myReturn :: a -> MyMaybe a
    myReturn = myPure

    myBind :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
    myBind MyNothing f = MyNothing
    myBind (MyJust a) f = f a