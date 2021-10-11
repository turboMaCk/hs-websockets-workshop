{-# LANGUAGE RecordWildCards #-}

module Connection (
    Connection,
    mkConnection,
    acceptData,
    receiveData,
    sendData,
) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import qualified Network.WebSockets as WS

data Connection = Connection
    { connectionSocket :: !WS.Connection
    , connectionId :: !Int
    }

mkConnection :: WS.Connection -> [Connection] -> Connection
mkConnection conn xs =
    Connection conn (length xs)

instance Eq Connection where
    c1 == c2 = connectionId c1 == connectionId c2

sendData :: ToJSON a => Connection -> a -> IO ()
sendData Connection{..} =
    WS.sendDataMessage connectionSocket . flip WS.Text Nothing . encode

receiveData :: FromJSON a => Connection -> IO (Maybe a)
receiveData Connection{..} =
    decode <$> WS.receiveData connectionSocket

acceptData :: FromJSON a => WS.Connection -> IO (Maybe a)
acceptData conn =
    decode <$> WS.receiveData conn
