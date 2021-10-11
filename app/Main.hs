{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (TVar)
import Control.Exception (Exception, finally, throwIO)
import Control.Monad
import Data.Text (Text)

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.WebSockets as WS

-- Local modules

import Connection (Connection)
import Session (Session (..))
import State (Message (..), State (..), User (..))

import qualified Connection as Connection
import qualified Session
import qualified State as State

main :: IO ()
main =
    STM.newTVarIO State.emptyState
        >>= (WS.runServer "127.0.0.1" 3000 . app)

data Error
    = SessionInvalid
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

addUser :: Text -> WS.Connection -> TVar State -> IO (Session, Connection, User)
addUser userName conn state = do
    session <- Session.mkSession
    putStrLn $ "Generated new Session " <> show session

    STM.atomically $
        let connection = Connection.mkConnection conn []
            userConnections = [connection]
         in do
                STM.modifyTVar' state $ \s@State{..} ->
                    s{stateUsers = HashMap.insert session User{..} stateUsers}
                pure (session, connection, User{..})

addConnection :: Session -> WS.Connection -> TVar State -> IO (Maybe (Connection, User))
addConnection session conn state =
    STM.atomically $ do
        s@State{..} <- STM.readTVar state
        let mUser = HashMap.lookup session stateUsers
        case mUser of
            Just user -> do
                let (newConnection, updatedUser) = State.userAddConnection conn user
                void $ STM.swapTVar state $
                    s{stateUsers = HashMap.insert session updatedUser stateUsers}
                pure $ Just (newConnection, updatedUser)
            Nothing ->
                pure Nothing

app :: TVar State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending

    putStrLn "Accepted new connection"

    WS.withPingThread conn 30 (pure ()) $ do
        -- handle existing session or create new user
        txt <- WS.receiveData @Text conn
        (session, connection, User{..}) <-
            case Text.stripPrefix "token:" txt of
                -- handle existing session
                Just token -> do
                    existing <- addConnection (Session $ token) conn state
                    case existing of
                        Nothing -> do
                            putStrLn $ "Unknown session " <> show txt
                            WS.sendClose @Text conn "Invalid token"
                            throwIO SessionInvalid
                        Just (connection, u) -> pure (Session token, connection, u)
                -- handle new user
                Nothing -> do
                    (session, connection, user) <- addUser txt conn state
                    putStrLn "Created new session"
                    WS.sendTextData conn $ Session.toText session
                    pure (session, connection, user)
        putStrLn $ "User connected " <> show userName

        -- send history
        State{..} <- STM.readTVarIO state
        Connection.sendData connection stateMessages

        -- chat handler
        handler state session User{..} connection

handler :: TVar State -> Session -> User -> Connection -> IO ()
handler state session User{..} connection = flip finally disconnect $
    forever $ do
        Just messageText <- Connection.receiveData @Text connection
        let messageUserName = userName
        broadcast Message{..} state
  where
    disconnect = do
        STM.atomically $ do
            STM.modifyTVar' state $ \s@State{..} ->
                s{stateUsers = HashMap.update (Just . (State.userRemoveConnection connection)) session stateUsers}

broadcast :: Message -> TVar State -> IO ()
broadcast message state = do
    connections <- STM.atomically $ do
        s@State{..} <- STM.readTVar state
        void $ STM.swapTVar state $ s{stateMessages = message : stateMessages}

        pure $ State.getAllConnections s

    forM_ connections $ \conn ->
        Connection.sendData conn message
