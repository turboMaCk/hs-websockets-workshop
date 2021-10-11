{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module State (
    User (..),
    Message (..),
    State (..),
    addConnection,
    removeConnection,
    getAllConnections,
    emptyState,
    addUser,
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

addConnection :: WS.Connection -> Session -> State -> Maybe (Connection, User, State)
addConnection conn session state@State{..} = addConn <$> HashMap.lookup session stateUsers
  where
    addConn user =
        let (connection, updatedUser) = userAddConnection conn user
         in ( connection
            , updatedUser
            , state{stateUsers = HashMap.insert session updatedUser stateUsers}
            )

removeConnection :: Session -> Connection -> State -> State
removeConnection session connection state@State{..} =
    state{stateUsers = HashMap.update (Just . userRemoveConnection connection) session stateUsers}

addUser :: WS.Connection -> Session -> Text -> State -> (Connection, User, State)
addUser conn session userName state@State{..} =
    ( connection
    , User{..}
    , state{stateUsers = HashMap.insert session User{..} stateUsers}
    )
  where
    connection = Connection.mkConnection conn []
    userConnections = [connection]
