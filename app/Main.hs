{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (TVar)
import Control.Monad
import Data.Text (Text)

import qualified Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

main :: IO ()
main = do
    STM.newTVarIO emptyState
        >>= (WS.runServer "127.0.0.1" 3000 . app)

data User = User
    { userName :: Text
    , userConnetions :: [WS.Connection]
    }

data State = State
    { stateUsers :: [User]
    }

emptyState :: State
emptyState =
    State []

app :: TVar State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $
        handler state conn

handler :: TVar State -> WS.Connection -> IO ()
handler state conn = forever $ do
    msg <- WS.receiveData @Text conn
    WS.sendTextData conn msg
