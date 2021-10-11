{-# LANGUAGE DeriveAnyClass #-}
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

import qualified Connection
import qualified State
import qualified Session

main :: IO ()
main =
    STM.newTVarIO State.emptyState
        >>= (WS.runServer "127.0.0.1" 3000 . app)

data Error
    = SessionInvalid
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

addConnection :: Session -> WS.Connection -> TVar State -> IO (Maybe (Connection, User))
addConnection session conn state =
    STM.atomically $ do
        s <- STM.readTVar state
        case State.addConnection conn session s of
            Just (connection, user, newState) -> do
                void $ STM.swapTVar state newState
                pure $ Just (connection, user)
            Nothing -> pure Nothing

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
                    existing <- addConnection (Session token) conn state
                    case existing of
                        Nothing -> do
                            putStrLn $ "Unknown session " <> show txt
                            WS.sendClose @Text conn "Invalid token"
                            throwIO SessionInvalid
                        Just (connection, u) -> pure (Session token, connection, u)
                -- handle new user
                Nothing -> do
                    (session, connection, user) <- do
                        session <- Session.mkSession
                        STM.atomically $ do
                            s <- STM.readTVar state
                            let (connection, user, newState) = State.addUser conn session txt s
                            STM.swapTVar state newState
                            pure (session, connection, user)

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
            STM.modifyTVar' state $ State.removeConnection session connection

broadcast :: Message -> TVar State -> IO ()
broadcast message state = do
    connections <- STM.atomically $ do
        s@State{..} <- STM.readTVar state
        void $ STM.swapTVar state $ s{stateMessages = message : stateMessages}

        pure $ State.getAllConnections s

    forM_ connections $ \conn ->
        Connection.sendData conn message
