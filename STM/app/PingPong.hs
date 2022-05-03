module PingPong where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent
import Control.Monad

-- Using TMVar as one-place communication channel

type Counter = TMVar Int

applyTMVar f c = do
    v <- takeTMVar c
    putTMVar c (f v)

swapIf :: (t -> Bool) -> TMVar t -> t -> STM ()
swapIf p c v = do
    oldV <- takeTMVar c
    if p oldV 
        then putTMVar c v
        else retry

t1 :: Counter -> IO ()
t1 c = do
    putStrLn "thread t1: started"
    threadDelay 1000000

    sequence_ $ repeat ( do
        threadDelay 700000
        atomically $ swapIf (==0) c 1
        putStrLn "thread t1: ping"
        )


t2 :: Counter -> IO ()
t2 c = do
    putStrLn "thread t2: started"
    threadDelay 1000000

    sequence_ $ repeat ( do
        threadDelay 1000000
        atomically $ swapIf (==1) c 0
        putStrLn "thread t2: pong"
        )

mainPingPong = do
    c <- newTMVarIO 0

    forkIO $ t1 c
    threadDelay 1000
    forkIO $ t2 c
    threadDelay 100000000