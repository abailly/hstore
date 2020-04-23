{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description of `StoredEvent`s which defines how arbitrary `Serializable` events are stored in underlying
--  storage engine.
module HStore.Events where

import qualified Data.ByteString as BS
import Data.Time
import HStore

newtype SHA1 = SHA1 {unSha1 :: BS.ByteString} deriving (Show, Eq)

defaultSha1 :: SHA1
defaultSha1 = SHA1 $ BS.replicate 20 0

-- | A `StoredEvent` is a basic unit of storage.
data StoredEvent a
  = StoredEvent
      { -- | Version of this event, useful to support migration and graceful upgrades of events
        eventVersion :: Version,
        -- | Timestamp for this event, a pair of (seconds,ns) since Epoch
        eventDate :: UTCTime,
        -- | Current source code version at time of event
        eventSHA1 :: SHA1,
        -- | The stored event
        event :: a
      }

instance Show s => Show (StoredEvent s) where
  show (StoredEvent v d s ev) = "StoredEvent " ++ show v ++ " " ++ show d ++ " " ++ show s ++ " " ++ show ev

instance Eq s => Eq (StoredEvent s) where
  (StoredEvent v d s ev) == (StoredEvent v' d' s' ev') = v == v' && d == d' && s == s' && ev == ev'
