{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Evaluator where

import Types
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy



newtype EvalM err state log a = EvalM ( ExceptT err (StateT state (Writer log)) a)
  deriving (Functor, Applicative, Monad, MonadError err, MonadState state, MonadWriter log)

runEvalM :: EvalM e s l a -> s -> ((Either e a, s), l)
runEvalM (EvalM m) initialState = runWriter $ runStateT (runExceptT m) initialState

runEval e = runEvalM e (C 0)

type Eval = EvalM EvalError Counter Log 

eval :: Term -> Eval Result
eval t@(Con n) = writer (n, line t (show n))
eval(Div t u) = do
    a <- eval t
    b <- eval u
    modify addOneC
    if b == 0 then do
                tell $ line (Div t u) "Error: divide by zero!"
                throwError $ DivideByZero
              else do
                tell $ line (Div t u) (show (div a b))
                return $ div a b

-- runEvalM (eval $ (Div (Con 3) (Con 0))) (C 0)
-- ((Left DivideByZero,C 1),Log "eval (Con 3) <= 3\neval (Con 0) <= 0\neval (Div (Con 3) (Con 0)) <= Error: divide by zero!\n")

-- runEvalM (eval $ (Div (Con 3) (Con 3))) (C 0)
-- ((Right 1,C 1),Log "eval (Con 3) <= 3\neval (Con 3) <= 3\neval (Div (Con 3) (Con 3)) <= 1\n")

----- STEPS 


-- adding Except
type EvalMEX = Except Error 

evalEX :: Term -> EvalMEX Result
evalEX (Con n) = return n
evalEX (Div t u) = do 
    a <- evalEX t
    b <- evalEX u
    if b == 0 then throwError $ Err "divide by zero"
              else return $ div a b

--- Examples
-- runExcept $ evalEX $ (Div (Con 3) (Div (Con 5) (Con 0)))
-- Left (Err "divide by zero")
-- 
-- runExcept  $ evalEX $ (Div (Con 16) (Div (Con 4) (Con 2)))
-- Right 8


-- adding State
-- preserve the state in error
type EvalMST = ExceptT Error (State Counter)

runEvalMST :: EvalMST a -> (Either Error a, Counter)
runEvalMST m =  runState (runExceptT m) (C 0)

evalST :: Term -> EvalMST Result
evalST (Con n) = return n
evalST (Div t u) = do
    modify addOneC
    a <- evalST t
    b <- evalST u
    if b == 0 then throwError $ Err "divide by zero"
              else return $ div a b

--- Examples
-- runEvalMST $ evalST $ (Div (Con 3) (Div (Con 5) (Con 0)))
-- (Left (Err "divide by zero"),C 2)
--
-- runEvalMST $ evalST $ (Div (Div (Con 64) (Con 2)) (Div (Con 6) (Con 3)))
-- (Right 16,C 3)

type EvalMST' = StateT Counter (Except Error)

runEvalMST' :: EvalMST' a -> (Either Error (a, Counter))
runEvalMST' m =  runExcept $ runStateT m (C 0)

evalST' :: Term -> EvalMST' Result
evalST' (Con n) = return n
evalST' (Div t u) = do
    modify addOneC
    a <- evalST' t
    b <- evalST' u
    if b == 0 then throwError $ Err "divide by zero"
              else return $ div a b

--- Examples
-- runEvalMST' $ evalST' $ (Div (Div (Con 64) (Con 2)) (Div (Con 6) (Con 3)))
-- Right (16,C 3)
-- 
-- runEvalMST' $ evalST' $ (Div (Div (Con 64) (Con 2)) (Div (Con 6) (Con 0)))
-- Left (Err "divide by zero")


-- add writer
type EvalMW = ExceptT Error (StateT Counter (Writer Log))

runEvalMW :: EvalMW a -> ((Either Error a, Counter), Log)
runEvalMW m = runWriter $ runStateT (runExceptT m) (C 0)

evalMW :: Term -> EvalMW Result
evalMW t@(Con n) = writer (n, line t (show n))
evalMW (Div t u) = do
    a <- evalMW t
    b <- evalMW u
    modify addOneC
    if b == 0 then do
                tell $ line (Div t u) "Error: divide by zero!"
                throwError $ Err "divide by zero"
              else do
                tell $ line (Div t u) (show (div a b))
                return $ div a b

--- Example
-- runEvalMW $ evalMW $ (Div (Div (Con 1972) (Con 6)) (Con 1))
-- ((Right 328,C 2),Log "eval (Con 1972) <= 1972\neval (Con 6) <= 6\neval (Div (Con 1972) (Con 6)) <= 328\neval (Con 1) <= 1\neval (Div (Div (Con 1972) (Con 6)) (Con 1)) <= 328\n")
-- 
-- runEvalMW $ evalMW $ (Div (Div (Con 64) (Con 2)) (Div (Con 6) (Con 0)))
-- ((Left (Err "divide by zero"),C 2),Log "eval (Con 64) <= 64\neval (Con 2) <= 2\neval (Div (Con 64) (Con 2)) <= 32\neval (Con 6) <= 6\neval (Con 0) <= 0\neval (Div (Con 6) (Con 0)) <= Error: divide by zero!\n")
--
-- runEvalMW $ evalMW $ (Div (Div (Con 64) (Con 0)) (Div (Con 6) (Con 3)))
-- ((Left (Err "divide by zero"),C 1),Log "eval (Con 64) <= 64\neval (Con 0) <= 0\neval (Div (Con 64) (Con 0)) <= Error: divide by zero!\n")


-- it's also possible to add IO

type EvalIO = ExceptT Error (StateT Counter (WriterT Log IO))

runEvalIO :: EvalIO a -> IO ((Either Error a, Counter), Log)
runEvalIO m = runWriterT $ runStateT (runExceptT m) (C 0)

evalIO :: Term -> EvalIO Result
evalIO t@(Con n) = writer (n, line t (show n))
evalIO (Div t u) = do
    liftIO $ print "test"
    a <- evalIO t
    b <- evalIO u
    modify addOneC
    if b == 0 then do
                tell $ line (Div t u) "Error: divide by zero!"
                throwError $ Err "divide by zero"
              else do
                tell $ line (Div t u) (show (div a b))
                return $ div a b

--- Example
-- runEvalIO $ evalIO (Div (Con 8) (Con 4))
-- "test"
-- ((Right 2,C 1),Log "eval (Con 8) <= 8\neval (Con 4) <= 4\neval (Div (Con 8) (Con 4)) <= 2\n")