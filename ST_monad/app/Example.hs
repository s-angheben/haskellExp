module Example where

import Control.Monad.ST
import Data.STRef
import GHC.Arr as A
import System.Random as R

swap :: STRef s a -> STRef s a -> ST s ()
swap v w = do 
    a <- readSTRef v
    b <- readSTRef w
    writeSTRef v b
    writeSTRef w a

hist bnds is = A.accumArray (+) 0 bnds
                [(i,1)|i<-is, A.inRange bnds i]

randomList :: Int -> Int -> Int -> IO [Int]
randomList n min max = sequence $ replicate n $ randomRIO (min, max)  

-- Example
-- hist (1,10) (take 10 . repeat $ 10)
-- > array (1,10) [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,10)]

-- list <- (randomList 1000 0 10)
-- hist (1, 10) list

binSort bnds key vs =
    A.accumArray (flip (:)) [] bnds
        [(key v, v)|v <- vs]

-- binSort (0,10) (flip div 100) <$> randomList 10 0 1000



