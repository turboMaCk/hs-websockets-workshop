{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (STM, TVar)
import Control.Exception (Exception, finally, throwIO)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
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
    STM.newTVarIO State.emptyState
        >>= (WS.runServer "127.0.0.1" 3000 . app)

data Join
    = JoinToken {joinToken :: Session}
    | JoinNew {joinName :: Text}
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

data Error
    = SessionInvalid
    | MessageInvalid
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

addConnection :: Session -> WS.Connection -> TVar State -> IO (Maybe (Connection, User))
addConnection session conn state = STM.atomically $ do
    s <- STM.readTVar state
    case State.addConnection conn session s of
        Nothing -> pure Nothing
        Just (connection, user, newState) -> do
            void $ STM.swapTVar state newState
            pure $ Just (connection, user)

joinApp :: TVar State -> WS.Connection -> IO (Session, Connection, User)
joinApp state conn = do
    msg <- Connection.acceptData conn
    case msg of
        Nothing -> throwIO MessageInvalid
        Just JoinToken{..} -> do
            existing <- addConnection joinToken conn state
            case existing of
                Nothing -> throwIO SessionInvalid
                Just (connection, u) -> pure (joinToken, connection, u)
        Just JoinNew{..} -> do
            session <- Session.mkSession
            STM.atomically $ do
                s <- STM.readTVar state
                let (connection, user, newState) = State.addUser conn session joinName s
                void $ STM.swapTVar state newState
                pure (session, connection, user)

app :: TVar State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $ do
        -- handle existing session or create new user
        (session, connection, User{..}) <- joinApp state conn
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
