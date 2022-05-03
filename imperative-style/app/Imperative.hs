module Imperative where

import GHC.Base
import Control.Monad

putsIO :: [Char] -> IO ()
putsIO [] = doneIO
putsIO (x:xs) = putChar x `seqIO` putsIO xs

seqIO :: IO a -> IO b -> IO b
seqIO ioa iob = ioa `bindIO` const iob

doneIO :: IO ()
doneIO = returnIO ()

-- using Control monad functions 
putsIO'' :: [Char] -> IO ()
putsIO'' = foldM_ (flip $ const . putChar) ()
--putsIO'' = foldM_ (\_ x -> putChar x) ()

-- using haskell do notation the code can be more clear, and similar to the imperative style

putsIO' :: [Char] -> IO ()
putsIO' [] = return ()
putsIO' (x:xs) = do
    putChar x
    putsIO' xs


seqIO' :: IO a -> IO b -> IO b
seqIO' ioa iob = do
    ioa
    iob

---- Other examples

echo :: IO ()
echo = do
    a <- getChar
    if a == '\n' then
        return ()
    else do
        putChar a
        echo

dup :: IO ()
dup = do
    c <- getChar 
    putChar c
    putChar c
        