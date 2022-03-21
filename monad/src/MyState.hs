module MyState where

import MyControl

newtype MState s a = MState {runState :: s -> (a, s)}

instance MyFunctor (MState s) where
    myFmap f (MState k) = MState (\s -> let (a, t) = k s
                                        in (f a, t))

instance MyApplicative (MState s) where
    myPure a = MState (\s -> (a, s))

    myApply (MState f) (MState a) = MState (\s -> let (g, t) = f s
                                                      (z, u) = a t
                                                  in (g z, u))

instance MyMonad (MState s) where
    myReturn = myPure

    myBind (MState k) f = MState (\s -> let (a, t) = k s
                                        in runState (f a) t) 


tick :: Num s => MState s ()
tick = MState (\s -> ((), s+1))
