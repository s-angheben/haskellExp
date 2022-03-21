module MyWriter where

import MyControl
import MyData

data MWriter o a = MWriter {output :: o, result :: a} 
    deriving Show
type Output = String 

toPair :: MWriter o a -> (o, a)
toPair (MWriter o a) = (o, a)

instance MyFunctor (MWriter o) where
    myFmap f (MWriter o a) = MWriter o (f a)

instance MyMonoid o => MyApplicative (MWriter o) where
    myPure = MWriter myMempty 
    
    myApply (MWriter o2 f) (MWriter o1 a) = MWriter (myMappend o1 o2) (f a)


instance MyMonoid o => MyMonad (MWriter o) where
    myReturn = myPure

    myBind ma k = let (x, a) = toPair ma
                      (y, b) = toPair $ k a
                  in MWriter (myMappend x y) b

out :: Monoid a => a -> MWriter a ()
out x = MWriter x ()