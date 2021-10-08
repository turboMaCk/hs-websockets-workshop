{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Session (
    Session (..),
    mkSession,
) where

import Control.Monad (replicateM)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Random as Rand

newtype Session = Session {unSession :: Text}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Hashable)

mkSession :: IO Session
mkSession =
    (Session . Text.pack)
        <$> replicateM 32 (Rand.randomRIO ('0', 'z'))
