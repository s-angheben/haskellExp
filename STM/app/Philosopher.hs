module Philosopher where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Data.Array.MArray
import System.Random
import Control.Monad

type Phil = Int
-- False  -> fork already taken
-- True   -> fork free 
type Forks = TArray Phil Bool 

data Table = Table {
    get_forks :: Forks,
    size  :: Int
}

myFork :: Phil -> Table -> (Int, Int)
myFork phil t 
        | phil == (size t) = (phil,1)
        | phil < 1         = error "out of range"
        | phil > (size t)  = error "out of range"
        | otherwise        = (phil, phil+1)

--- map (flip myFork a) [1..5]
-- [(0,1),(1,2),(2,3),(3,4),(4,0)]

take_one :: Int -> Table -> STM ()
take_one pos table = do
    let forks = get_forks table
    state <- readArray forks pos
    case state of
        False -> retry                       -- retry the fork is occupied
        True  -> writeArray forks pos False  -- take the fork

take_forks :: Phil -> Table -> STM ()
take_forks p table = do
    let (left_fork, right_fork) = myFork p table
    take_one left_fork table
    take_one right_fork table

 
put_forks :: Phil -> Table -> STM ()
put_forks p table = do
    let (left_fork, right_fork) = myFork p table
        forks = get_forks table
    release left_fork forks
    release right_fork forks
  where
    release :: Int -> Forks -> STM ()
    release pos forks = writeArray forks pos True

createTable :: Int -> STM Table
createTable n = do 
    forks <- newArray (1,n) True
    return $ Table forks n

think = do 
    r <- randomRIO (100000,500000)
    threadDelay r

eat = do 
    r <- randomRIO (100000,500000)
    threadDelay r

philosopher :: Table -> Phil -> IO ()
philosopher table i = sequence_ $ repeat (do
    think
    atomically $ take_forks i table
    eat
    atomically $ put_forks i table
    putStrLn $ "philosopher: " ++ show i ++ " ok" )

mainp :: IO ()
mainp = do
    table <- atomically $ createTable 5
    forM_ [1..5] (\id -> forkIO $ philosopher table id)
