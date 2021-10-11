{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module State (
    User (..),
    Message (..),
    State (..),
    userAddConnection,
    userRemoveConnection,
    getAllConnections,
    emptyState,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics

import qualified Data.HashMap.Strict as HashMap
import qualified Network.WebSockets as WS

import Connection (Connection, mkConnection)
import Session (Session (..))

data User = User
    { userName :: !Text
    , userConnections :: ![Connection]
    }

userAddConnection :: WS.Connection -> User -> (Connection, User)
userAddConnection conn user =
    let connection = mkConnection conn (userConnections user)
     in (connection, user{userConnections = connection : userConnections user})

userRemoveConnection :: Connection -> User -> User
userRemoveConnection conn user =
    user{userConnections = filter (conn /=) $ userConnections user}

data Message = Message
    { messageUserName :: !Text
    , messageText :: !Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data State = State
    { stateUsers :: !(HashMap Session User)
    , stateMessages :: ![Message]
    }

emptyState :: State
emptyState =
    State HashMap.empty []

getAllConnections :: State -> [Connection]
getAllConnections State{..} =
    concat $ userConnections <$> stateUsers
