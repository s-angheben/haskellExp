{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Part1 where

import Data.Kind

data Sum a b  = L a | R b 
data Prod a b = Prod a b 


data Tree a = Leaf a | Node (Tree a) (Tree a)
data Baum a = Blatt a | Knoten (Baum a) (Baum a) 

-- nominal type system: these 2 types are different but they have the same structure

-- IDEA: introduce a rapresentation datatypes that is ismorphic to the original type

-- used to implement equality
newtype Wrap a = Wrap a

class Generic a where
    type Rep a :: Type

    from :: a -> Rep a
    to   :: Rep a -> a

-- to . from = id

instance Generic (Tree a) where
    type Rep (Tree a) = Sum (Wrap a) (Prod (Wrap (Tree a)) (Wrap (Tree a)))

    from :: Tree a -> Rep (Tree a)
    from (Leaf x) = L (Wrap x)
    from (Node l r) = R (Prod (Wrap l) (Wrap r))

    to :: Rep (Tree a) -> Tree a
    to (L (Wrap x)) = Leaf x
    to (R (Prod (Wrap l) (Wrap r))) = Node l r


-- define eq, we just need from

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq a1 a2 = geq (from a1) (from a2)

class GEq a where
    geq :: a -> a -> Bool

instance (GEq a, GEq b) => GEq (Sum a b) where
    geq (L a1) (L a2) = geq a1 a2
    geq (R b1) (R b2) = geq b1 b2
    geq _       _     = False

instance (GEq a, GEq b) => GEq (Prod a b) where
    geq (Prod a1 b1) (Prod a2 b2) = geq a1 a2 && geq b1 b2

instance Eq a => GEq (Wrap a) where
  geq (Wrap a1) (Wrap a2) = a1 == a2

instance Eq a => Eq (Tree a) where
    (==) = eq

---- examples
-- eq (Leaf 4) (Leaf 4)
-- True
-- eq (Leaf 4) (Leaf 3)
-- False
-- eq (Leaf 4) (Node (Leaf 3) (Leaf 6))
-- False

-- Baum test
instance Generic (Baum a) where
    type Rep (Baum a) = Sum (Wrap a) (Prod (Wrap (Baum a)) (Wrap (Baum a)))

    from :: Baum a -> Rep (Baum a)
    from (Blatt x) = L (Wrap x)
    from (Knoten l r) = R (Prod (Wrap l) (Wrap r))

    to :: Rep (Baum a) -> Baum a
    to (L (Wrap x)) = Blatt x
    to (R (Prod (Wrap l) (Wrap r))) = Knoten l r

instance Eq a => Eq (Baum a) where
    (==) = eq

-- I think that since recursion is not fix we can't compare Baum and Tree in this way
-- geq (from (Leaf 4)) (from (Blatt 4))



--- EXERCISE
-- 1) Define a Generic instance for Term

data Term = App Term Term
          | Abs String Term
          | Var String


instance Generic Term where
    type Rep Term = Sum (Prod (Wrap Term) (Wrap Term)) 
                        (Sum (Prod (Wrap String) (Wrap Term)) (Wrap String))

    from :: Term -> Rep Term
    from (App t1 t2) = L $ Prod (Wrap t1) (Wrap t2)
    from (Abs s t)   = R $ L $ Prod (Wrap s) (Wrap t)
    from (Var s)     = R $ R (Wrap s)

    to :: Rep Term -> Term 
    to (L (Prod (Wrap t1) (Wrap t2))) = App t1 t2
    to (R (L (Prod (Wrap s) (Wrap t)))) = Abs s t
    to (R (R (Wrap s)))                 = Var s

instance Eq Term where
    (==) = eq

--- examples
-- eq (Var "ciao") (Var "test")
-- False
-- eq (Var "ciao") (Var "ciao")
-- True
-- eq (Var "ciao") (Abs "ciao" (Var "ciao"))
-- False

-- 2) Define Generic comparison

cpm :: (Generic a, GCmp (Rep a)) => a -> a -> Ordering
cpm a1 a2 = gcpm (from a1) (from a2)

class GCmp a where
    gcpm :: a -> a -> Ordering

instance (GCmp a, GCmp b) => GCmp (Sum a b) where
    gcpm (L a1) (L a2) = gcpm a1 a2
    gcpm (R b1) (R b2) = gcpm b1 b2
    gcpm _      _      = undefined -- can be implemented in different ways

instance (GCmp a, GCmp b) => GCmp (Prod a b) where
    gcpm (Prod a1 b1) (Prod a2 b2) = gcpm a1 a2 -- in this case we give priority to the first arg
--    gcpm (Prod a1 b1) (Prod a2 b2) = gcpm b1 b2


instance Ord a => GCmp (Wrap a) where
  gcpm (Wrap a1) (Wrap a2) = compare a1 a2

instance Ord a => Ord (Tree a) where
    compare = cpm

instance Ord Term where
    compare = cpm

-- examples
-- compare (Leaf 4) (Leaf 9)
-- LT
-- compare (Var "f") (Var "b")
-- GT

-------------------------------------------------------------
-- define a generic operation that enumerates all values of a datatype as a list
-- in this case we need to use to

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

class GEnum a where
    genum :: [a]

data Colour = Red | Green | Blue
    deriving Show

instance Generic Colour where
    type Rep Colour = Sum () (Sum () ())

    from :: Colour -> Rep Colour
    from Red   = L ()
    from Green = R $ L () 
    from Blue  = R $ R ()

    to :: Rep Colour -> Colour
    to (L ())     = Red
    to (R (L ())) = Green
    to (R (R ())) = Blue

instance GEq () where
    geq () () = True

instance Eq Colour where
    (==) = eq

instance GEnum () where
    genum = [()]

instance (GEnum a, GEnum b) => GEnum (Sum a b) where
    genum = (L <$> genum) ++ (R <$> genum)

--- example
-- enum @Colour
-- [Red,Green,Blue]

-- 3) define GBaseCases
{-
data TestBase a = A
                | B a

instance Generic (TestBase a) where
    type Rep (TestBase a) = Sum () (Wrap a)

    from :: TestBase a -> Rep (TestBase a)
    from A     = L ()
    from (B a) = R (Wrap a)
    
    to :: Rep (TestBase a) -> TestBase a
    to (L ()) = A
    to (R (Wrap a)) = B a


baseCases :: (Generic a, GBaseCases (Rep a)) => [a]
baseCases = to <$> gbaseCases

class GBaseCases a where
    gbaseCases :: b -> [a]
-}