{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (STM, TVar)
import Control.Exception (Exception, finally, throwIO)
import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.List (intersperse)
import Data.Text (Text)

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.WebSockets as WS

-- Local modules

import Session (Session (..))

import qualified Session

main :: IO ()
main =
    STM.newTVarIO emptyState
        >>= (WS.runServer "127.0.0.1" 3000 . app)

data Connection = Connection
    { connectionSocket :: !WS.Connection
    , connectionId :: !Int
    }

instance Eq Connection where
    c1 == c2 = connectionId c1 == connectionId c2

data User = User
    { userName :: !Text
    , userConnections :: ![Connection]
    }

addConn :: WS.Connection -> User -> (Connection, User)
addConn conn user =
    let connection = Connection conn (length (userConnections user))
     in (connection, user{userConnections = connection : userConnections user})

removeConn :: Connection -> User -> User
removeConn conn user =
    user { userConnections = filter (conn /=) $ userConnections user }

data Message = Message
    { messageUserName :: !Text
    , messageText :: !Text
    }

render :: Message -> Text
render Message{..} =
    messageUserName <> ": " <> messageText

renderList :: [Message] -> Text
renderList xs =
    "[\"" <> (Text.concat $ intersperse "\",\"" (render <$> xs)) <> "\"]"

instance Show Message where
    show Message{..} =
        show messageUserName <> ": " <> show messageText

data State = State
    { stateUsers :: !(HashMap Session User)
    , stateMessages :: ![Message]
    }

getConnections :: State -> [WS.Connection]
getConnections State{..} =
    fmap connectionSocket $ concat $ userConnections <$> stateUsers

emptyState :: State
emptyState =
    State HashMap.empty []

data Error
    = SessionInvalid
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

addUser :: Text -> WS.Connection -> TVar State -> IO (Session, Connection, User)
addUser userName conn state = do
    session <- Session.mkSession
    putStrLn $ "Generated new Session " <> show session

    STM.atomically $
        let connection = Connection conn 0
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
                let (newConnection, updatedUser) = addConn conn user
                STM.swapTVar state $
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
        WS.sendTextData conn $ renderList stateMessages

        -- chat handler
        handler state session User{..} connection

handler :: TVar State -> Session -> User -> Connection -> IO ()
handler state session User{..} Connection{..} = flip finally disconnect $
    forever $ do
        messageText <- WS.receiveData @Text connectionSocket
        let messageUserName = userName
        broadcast Message{..} state
  where
    disconnect = do
        STM.atomically $ do
            STM.modifyTVar' state $ \s@State{..} ->
                s{stateUsers = HashMap.update (Just . (removeConn Connection{..})) session stateUsers}

broadcast :: Message -> TVar State -> IO ()
broadcast message state = do
    connections <- STM.atomically $ do
        s@State{..} <- STM.readTVar state
        STM.swapTVar state $ s{stateMessages = message : stateMessages}

        pure $ getConnections s

    forM_ connections $ \conn ->
        WS.sendTextData conn $ render message
