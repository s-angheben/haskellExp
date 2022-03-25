module Graph where

import GHC.Arr as A
import Control.Monad (forM_)
import GHC.ST
import qualified Data.STRef as A
import Data.STRef

-- a Node contains the id (Int), a value (a) and a list of adj node id
data Node a = Node {nodeId :: Int,
                    value :: a,
                    adj :: [Int]}

newtype Graph a = Graph [Node a]
type GraphInt = Graph Int

exampleGraph :: GraphInt
exampleGraph = Graph [Node 0 0 [2, 4], Node 1 1 [4], Node 2 2 [4], Node 3 3 [2], Node 4 4 [3]]

graphSize :: Int
graphSize = 4

dfsrec graph@(Graph nodes) visited dfsList currentNodeId counter = do
    addandSet visited dfsList currentNodeId counter

    forM_ (adj (nodes !! currentNodeId)) (\adjNode -> do
        visitState <- A.readSTArray visited adjNode
        if visitState then return () 
        else dfsrec graph visited dfsList adjNode counter)


dfs graph n startNodeId = do
    visited <- A.newSTArray (0,n) False
    dfsList <- A.newSTArray (0,n) (-1)
    counter <- newSTRef 0

    dfsrec graph visited dfsList startNodeId counter

    A.freezeSTArray dfsList


addandSet visited dfsList nodeId counter = do
    A.writeSTArray visited nodeId True
    n <- readSTRef counter
    A.writeSTArray dfsList n nodeId
    writeSTRef counter (n+1)


test = runST (dfs exampleGraph graphSize 0)