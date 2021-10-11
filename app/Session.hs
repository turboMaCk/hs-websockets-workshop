{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Session (
    Session (..),
    mkSession,
    toText,
) where

import Control.Monad (replicateM)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Random as Rand

newtype Session = Session {unSession :: Text}
    deriving stock (Eq, Ord)
    deriving newtype (Hashable)
    deriving (Show) via Text

mkSession :: IO Session
mkSession =
    Session . Text.pack
        <$> replicateM 32 (Rand.randomRIO ('0', 'z'))

toText :: Session -> Text
toText = unSession
