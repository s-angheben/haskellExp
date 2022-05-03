module Broadcast where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Concurrent
import System.Random
import Control.Exception

type MyChan = TChan String


client :: MyChan -> Int -> IO ()
client bchan n = do
    cchan <- atomically $ dupTChan bchan
    forever $ do
        msg <- atomically $ readTChan cchan
        randomDelay
        putStrLn $ "client" ++ show n ++ ": " ++ msg

mainChan :: IO ()
mainChan = do
    bchan <- newBroadcastTChanIO

    tIds <- mapM (forkIO . client bchan) [1..2]

    loop bchan

    putStrLn "killing threads, bye."
    mapM_ killThread tIds

    where
        loop :: MyChan -> IO ()
        loop bchan = do
            putStr "insert command: "
            s <- getLine
            case s of
                "send"        -> do        
                                send bchan
                                threadDelay 1000000
                                loop bchan
                "exit"        -> return ()
                _             -> do
                                putStrLn "command not found."
                                loop bchan
        
        send bchan = do 
            putStr "insert msg: "
            getLine >>= atomically . writeTChan bchan

randomDelay = do
    r <- randomRIO (100000,500000)
    threadDelay r