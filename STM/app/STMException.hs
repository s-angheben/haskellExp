module STMException where

import Control.Monad.STM
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad

type Msg = TMVar String

data Ex = StringTooShort
    deriving Show

instance Exception Ex

catchOutside :: Foldable t => TMVar (t a) -> STM ()
catchOutside msg = do
    m <- takeTMVar msg
    when (length m < 10) $ throwSTM StringTooShort


handler StringTooShort = return ()

-- only in the second case the value is taken 
-- bc in the first one the actions are rolled-back
mainExOut :: IO ()
mainExOut = do
    msg <- newTMVarIO "a"

    -- Even if the throwInside take the msg and put nothing in it then because of the throwSTM  
    -- all STM action inside are rolled-back! since the handler is not inside the STM action

    atomically $ catchSTM (catchOutside msg) handler
    m <- atomically $ tryReadTMVar msg
    putStrLn $ "msg: " ++ show m

    atomically $ swapTMVar msg "longString"

    -- Now the exeption isn't throw so the value is taken
    atomically $ catchSTM (catchOutside msg) handler
    m <- atomically $ tryReadTMVar msg
    putStrLn $ "msg: " ++ show m


catchInside msg = do
    m <- takeTMVar msg
    when (length m < 10) $ catchSTM (throwSTM StringTooShort) handler



-- In both case the value is taken
mainExIn :: IO ()
mainExIn = do
    msg <- newTMVarIO "a"

    -- Since the catchSTM is inside the STM action even if the string
    -- is short and the throwSTM is called the actions will not be rolled-back
    atomically $ catchInside msg
    m <- atomically $ tryReadTMVar msg
    putStrLn $ "msg: " ++ show m

    -- Now we need to put something in the msg, because the previous operation
    -- took the value
    atomically $ putTMVar msg "longString"

    atomically $ catchInside msg
    m <- atomically $ tryReadTMVar msg
    putStrLn $ "msg: " ++ show m