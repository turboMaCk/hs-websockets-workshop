{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Session (
    Session (..),
    mkSession,
    toText,
) where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified System.Random as Rand

newtype Session = Session {unSession :: Text}
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)
    deriving (Show) via Text

mkSession :: IO Session
mkSession =
    Session . Text.pack
        <$> replicateM 32 (Rand.randomRIO ('0', 'z'))

toText :: Session -> Text
toText = unSession
