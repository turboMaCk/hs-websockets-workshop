{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (STM, TVar)
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

data User = User
    { userName :: !Text
    , userConnections :: ![WS.Connection]
    }

data Message = Message
    { messageUserName :: !Text
    , messageText :: !Text
    }

instance Show Message where
    show Message{..} =
        show messageUserName <> ": " <> show messageText

data State = State
    { stateUsers :: !(HashMap Session User)
    , stateMessages :: ![Message]
    }

renderList :: Show a => [a] -> Text
renderList xs =
    "[" <> Text.pack $ concat $ intersperse "," (show <$> xs) <> "]"

emptyState :: State
emptyState =
    State HashMap.empty []

addUser :: Text -> WS.Connection -> TVar State -> IO User
addUser userName conn state = do
    session <- Session.mkSession
    putStrLn $ "Generated new Session " <> show session

    STM.atomically $
        let userConnections = [conn]
         in do
                STM.modifyTVar' state $ \s@State{..} ->
                    s{stateUsers = HashMap.insert session User{..} stateUsers}
                pure User{..}

addConnection :: Session -> WS.Connection -> TVar State -> IO (Maybe User)
addConnection session conn state =
    STM.atomically $ do
        s@State{..} <- STM.readTVar state
        let mUser = HashMap.lookup session stateUsers
        case mUser of
            Just user -> do
                let updatedUser = user{userConnections = conn : userConnections user}
                STM.swapTVar state $
                    s{stateUsers = HashMap.insert session updatedUser stateUsers}
                pure $ Just updatedUser
            Nothing ->
                pure Nothing

app :: TVar State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending

    putStrLn "Accepted new connection"

    WS.withPingThread conn 30 (pure ()) $ do
        txt <- WS.receiveData @Text conn
        User{..} <- do
            existing <- addConnection (Session txt) conn state
            case existing of
                Nothing -> addUser txt conn state
                Just u -> pure u

        putStrLn $ "User connected " <> show userName

        State{..} <- STM.readTVarIO state
        -- send history
        WS.sendTextData conn $ renderList stateMessages
        handler state conn

handler :: TVar State -> WS.Connection -> IO ()
handler state conn = forever $ do
    msg <- WS.receiveData @Text conn
    WS.sendTextData conn msg
