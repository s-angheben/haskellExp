{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
module ResourceManager where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad

-- resource manager

type Resource = TVar Int

createResource :: STM (TVar Int)
createResource = newTVar 0

putR :: Resource -> Int -> STM ()
putR r i = do
    v <- readTVar r
    writeTVar r (v+i)

getR :: Resource -> Int -> STM ()
getR r i = do
    v <- readTVar r
    if v<i then retry
           else writeTVar r (v-i)

nonBlockGetR :: Resource -> Int -> STM Bool
nonBlockGetR r i = (getR r i >> return True)
                   `orElse` return False

blockGetR :: Resource -> Int -> STM ()
blockGetR r i = do
    s <- nonBlockGetR r i
    if s then return () else retry

t1 :: Resource -> Int -> IO ()
t1 r n = do
    putStrLn "thread 1: started"
    putStrLn "thread 1: asking 3 resources"
    atomically $ getR r n
    threadDelay 1000
    putStrLn $ "thread 1: retrived " ++ show n ++ " resources"

giveRes r = do
    atomically (putR r 1)
    putStrLn "put 1 resource"
    sleep 2

testResourceManager :: IO ()
testResourceManager = do
    putStrLn "test resource manager"

    resource <- atomically createResource
    putStrLn "resource created"

    -- thread t1 ask 3 resources
    forkIO $ t1 resource 3
    threadDelay 1000

    -- main put resource
    replicateM_ 4 (giveRes resource)


sleep = threadDelay . (*1000000)

t1' :: Resource -> IO ()
t1' r =  do
    putStrLn "thread: created"
    putStrLn "thread: try get resource"
    result <- atomically $ nonBlockGetR r 1
    putStrLn $ "thread: got resource=" ++ show result


    putStrLn "thread: blocking on resource"
    atomically $ blockGetR r 1
    putStrLn "thread: got resource"


testResourceManager' :: IO ()
testResourceManager' = do
    r <- atomically createResource

    forkIO $ t1' r

    sleep 2
    atomically (putR r 1)
    sleep 1

    v <- readTVarIO r
    putStrLn $ "resource value=" ++ show v


-- test composition 

thread1 r1 n1 r2 n2 = do
    putStrLn "thread1: created"
    putStrLn "thread1: asking r1 and r2"
    atomically (do
        getR r1 n1
        getR r2 n2
        )
    putStrLn "thread1: got r1 and r2"

thread2 r1 n1 = do
    putStrLn "thread2: created"
    putStrLn "thread2: asking r1"
    atomically (getR r1 n1)
    putStrLn "thread2: using r1"
    atomically (putR r1 n1)
    putStrLn "thread2: put r1"

thread3 r1 n1 r2 n2 = do
    putStrLn "thread3: created"
    putStrLn "thread3: asking r1 or r2"
    taken <- atomically (getRInfo r1 n1 `orElse` getRInfo r2 n2)
    if taken == r1
        then do
            putStrLn "thread3: using r1"
            atomically $ putR r1 n1
        else do
            putStrLn "thread3: using r2"
            atomically $ putR r2 n2

getRInfo :: Resource -> Int -> STM Resource
getRInfo r i = do
    v <- readTVar r
    if v<i then retry
           else do
               writeTVar r (v-i)
               return r

test2 :: IO ()
test2 = do
    r1 <- atomically createResource
    r2 <- atomically createResource

    forkIO $ thread1 r1 3 r2 7
    threadDelay 1000
    forkIO $ thread2 r1 3
    threadDelay 1000
    forkIO $ thread3 r1 2 r2 7
    threadDelay 1000

    sleep 1
    atomically $ putR r1 2
{-
    sleep 3
    v1 <- readTVarIO r1
    print v1
    v2 <- readTVarIO r1
    print v1
-}
    sleep 1
    atomically $ putR r1 1
    sleep 1
    atomically $ putR r2 7
    sleep 4

-- should run t3 first, then r2 and then r1