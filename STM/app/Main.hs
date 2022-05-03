module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Control.Concurrent
import Control.Monad
import qualified ResourceManager as RM

main :: IO ()
main = RM.testResourceManager 