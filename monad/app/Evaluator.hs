module Evaluator where

import MyControl
import MyException
import MyState
import MyWriter

import MyType



-- We would have 
-- + Exception to catch the divide by zero error
-- + State to count the number of recursive computation
-- + Output to log the actions


-------------- BASE

evalBase :: Term -> Int
evalBase (Con a) = a
evalBase (Div t u) = div (evalBase t) (evalBase u)

answerExpr, errorExpr :: Term
answerExpr = Div (Div (Con 1972) (Con 2)) (Con 23)
errorExpr  = Div (Con 1) (Con 0)

-------------- EXCEPTION RAW

data ExBase a = RaiseExBase Excetion | ReturnExBase a
    deriving Show
type Excetion = String

evalExBase :: Term -> ExBase Int
evalExBase (Con a) = ReturnExBase a
evalExBase (Div t u) = case evalExBase t of
                    RaiseExBase e  -> RaiseExBase e
                    ReturnExBase a ->
                        case evalExBase u of
                            RaiseExBase e  -> RaiseExBase e
                            ReturnExBase b ->
                                if b == 0 then RaiseExBase "divide by zero"
                                          else ReturnExBase (div a b)

-------------- STATE RAW

type StateTypeBase = Int

evalStateBase :: Term -> StateTypeBase -> (Int, StateTypeBase)
evalStateBase (Con a) x = (a, x)
evalStateBase (Div t u) x = let (a, y) = evalStateBase t x
                                (b, z) = evalStateBase u y
                            in
                                (div a b, z+1)

-------------- OUTPUT RAW

type LogBase a = (OutputType, a)
type OutputType = String

evalOutputBase :: Term -> LogBase Int
evalOutputBase (Con a) = (line (Con a) a, a)
evalOutputBase (Div t u) = let (x, a) = evalOutputBase t
                               (y, b) = evalOutputBase u
                           in
                               (x ++ y ++ line (Div t u) (div a b), div a b)
-- reverse                     (line (Div t u) (div a b) ++ y ++ x, div a b)   

line :: Term -> Int -> OutputType
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"


------------ MONADIC VERSION
-- evalM :: Term -> M Int
-- evalM (Con a) = return a
-- evalM (Div t u) = eval t >>= 
--    \a -> eval u >>= 
--        \b -> return (div a b)

--- using my types the monadic evaluation function has the form
------------------------------------------------------------------------------------
-- evalM (Con a)   = myReturn a
-- evalM (Div t u) = myBind (evalME t) (\a -> myBind (evalME u) (\b -> specif a b))
-- {equals}
-- evalM (Div t u) = myBind (evalME t) (myBind (evalME u) . specif))
------------------------------------------------------------------------------------
-- where specific is a specific function depending on the monad used

-- Exception Monad

evalME :: Term -> MEx Int
evalME (Con a) = myReturn a
evalME (Div t u) = myBind (evalME t) (myBind (evalME u) . test)
    where
        test :: Integral a => a -> a -> MEx a
        test a b = if b == 0 then raise "divide by zero"
                             else myReturn (div a b)

-- State Monad
evalMS :: Term -> MState Int Int
evalMS (Con a) = myReturn a
evalMS (Div t u) = myBind (evalMS t) (myBind (evalMS u) . count)
    where
        count :: Int -> Int -> MState Int Int
        count a b = myBind tick (\() -> myReturn (div a b))

-- Writer Monad
evalMW :: Term -> MWriter OutputType Int
evalMW (Con a)   = myBind (out $ line (Con a) a) (\_ -> myReturn a)
evalMW (Div t u) = myBind (evalMW t) (myBind (evalMW u) . log)
    where 
        log a b = myBind (out $ line (Div t u) (div a b)) (\_ -> myReturn (div a b))
