{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |

Steps:
------
1. Implement simple echo service
    - use Message type mock for compatibility
2. Add handling for Users and Connections (Join message)
    - use SessionCreated for sending token
3. Implement handling of Error
4. Implement broadcast and message sending (real state)
5. Implement disconnect
6. add SyncMessages to sync state

Front-end communication flow:
-----------------------------
- accept connection
- start ping thread
- read data @Join
   - can fail with MessageInvalid
   - JoinToken can fail with SessionInvalid
   - JoinNew should send SessionCreated back right away
- Send [Message]
- start forever handler receive @Text (messageText) -> broadcast @Message

Exception handling:
-------------------
- handle Error exception after accepting connection
- after storing connection to State handle disconnect using finally

Types needed for implementation (compatibility with front-end code):
--------------------------------------------------------------------
data Join
    = JoinToken {joinToken :: Session}
    | JoinNew {joinName :: Text}
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

data Error
    = SessionInvalid
    | MessageInvalid
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Exception, FromJSON, ToJSON)

data Event
    = SessionCreated {newToken :: Session}
    | SyncMessages {syncMessages :: [Message]}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
-}
module Main where

import Control.Concurrent.STM (STM, TVar)
import Control.Exception (Exception, finally, handle, throwIO)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics

import qualified Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

-- Local modules

import Connection (Connection)
import Session (Session (..))
import State (Message (..), State (..), User (..))

import qualified Connection
import qualified Session
import qualified State

main :: IO ()
main =
    WS.runServer "127.0.0.1" 4000 app


app :: WS.ServerApp
app pendingConn = do
    conn <- WS.acceptRequest pendingConn
    WS.withPingThread conn 30 (pure ()) $ forever $ do
        msg <- Connection.acceptData @Text conn
        case msg of
          Just txt -> Connection.sendSocket conn $ Message "you" txt
          Nothing -> putStrLn "I don't understand"
