{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
-}
main :: IO ()
main =
    STM.newTVarIO State.emptyState
        >>= (WS.runServer "127.0.0.1" 4000 . app)

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

addConnection :: Session -> WS.Connection -> TVar State -> STM (Maybe (Connection, User))
addConnection session conn state = do
    s <- STM.readTVar state
    case State.addConnection conn session s of
        Nothing -> do
            pure Nothing
        Just (connection, user, newState) -> do
            void $ STM.swapTVar state newState
            pure $ Just (connection, user)

addUser :: Session -> WS.Connection -> Text -> TVar State -> STM (Connection, User)
addUser session conn joinName state = do
    s <- STM.readTVar state
    let (connection, user, newState) = State.addUser conn session joinName s
    void $ STM.swapTVar state newState
    pure (connection, user)

joinApp :: TVar State -> WS.Connection -> IO (Session, Connection, User)
joinApp state conn = do
    msg <- Connection.acceptData conn
    putStrLn "join request"
    case msg of
        Nothing -> throwIO MessageInvalid
        Just JoinToken{..} -> do
            existing <- STM.atomically $ addConnection joinToken conn state
            case existing of
                Nothing -> throwIO SessionInvalid
                Just (connection, user) -> pure (joinToken, connection, user)
        Just JoinNew{..} -> do
            session <- Session.mkSession
            Connection.sendSocket conn $ SessionCreated session
            (connection, user) <- STM.atomically $ addUser session conn joinName state
            pure (session, connection, user)

errHandler :: WS.Connection -> Error -> IO ()
errHandler conn err = do
    putStrLn $ "Error occured " <> show err
    Connection.sendSocket conn err

app :: TVar State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $
        handle (errHandler conn) $ do
            -- handle existing session or create new user
            (session, connection, User{..}) <- joinApp state conn

            putStrLn $ "User connected " <> show userName

            -- send history
            State{..} <- STM.readTVarIO state
            Connection.sendData connection $ SyncMessages stateMessages

            -- chat handler
            handler state session User{..} connection

handler :: TVar State -> Session -> User -> Connection -> IO ()
handler state session User{..} connection = flip finally disconnect $
    forever $ do
        msg <- Connection.receiveData @Text connection
        case msg of
            Just messageText ->
                let messageUserName = userName
                 in broadcast Message{..} state
            Nothing -> do
                putStrLn "Can't parse messageText"
  where
    disconnect = do
        putStrLn $ "disconnecting " <> show userName
        STM.atomically $ do
            STM.modifyTVar' state $ State.removeConnection session connection

broadcast :: Message -> TVar State -> IO ()
broadcast message state = do
    connections <- STM.atomically $ do
        s@State{..} <- STM.readTVar state
        void $ STM.swapTVar state $ s{stateMessages = message : stateMessages}

        pure $ State.getAllConnections s

    forM_ connections $ \conn ->
        Connection.sendData conn message
