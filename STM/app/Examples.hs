module Examples where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Control.Concurrent
import Control.Monad

-- simple use of forkIO
{-
sayHello :: String -> IO ()
sayHello = putStrLn . (++ " ciao!")


main :: IO ()
main = do
    name <- getLine
    forkIO (sayHello name)
    forkIO (sayHello name)
    return ()
-}
-- simple use of TQueue
{-
main :: IO ()
main = do
    messages <- atomically $ do
        msg <- newTQueue
        writeTQueue msg "buffered"
        writeTQueue msg "queue"
        return msg

    putStrLn =<< (atomically . readTQueue) messages
    putStrLn =<< (atomically . readTQueue) messages
-}

-- 2 thread reading the same TQueue
{-
type QAlbum = TQueue Int

readAlbum :: QAlbum -> IO ()
readAlbum q = 
    atomically (readTQueue q) >>= print

main :: IO ()
main = do
    albumsQueue <- atomically newTQueue

    forkIO $ readAlbum albumsQueue
    forkIO $ readAlbum albumsQueue

    atomically $ writeTQueue albumsQueue 5

    return ()
-}

-- Example above with input

type QAlbum = TQueue String

readAlbum :: QAlbum -> IO ()
readAlbum q = do
    album <- atomically (readTQueue q)
    tId <- myThreadId
    putStrLn $ show tId <> ": READ: " <> album

loop q = do
    putStr "Insert new album: "
    album <- getLine
    when (album /= "exit") $ do
            atomically (writeTQueue q album)
            threadDelay 1000000
            loop q

do10times :: Monad m => m a -> m ()
do10times = replicateM_ 10

main :: IO ()
main = do
    albumsQueue <- atomically newTQueue

    t1 <- forkIO $ do10times $ readAlbum albumsQueue
    putStrLn $ "Thread " <> show t1 <> " created!"
    t2 <- forkIO $ do10times $ readAlbum albumsQueue
    putStrLn $ "Thread " <> show t2 <> " created!"

    atomically $ writeTQueue albumsQueue "test"
    threadDelay 1000000

    putStrLn "start loop"
    loop albumsQueue

    killThread t1
    killThread t2