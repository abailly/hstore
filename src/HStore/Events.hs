{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description of `StoredEvent`s which defines how arbitrary `Serializable` events are stored in underlying
--  storage engine.
module HStore.Events where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Instances.Time()
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Time

newtype Version = Version {version :: Word64}
  deriving (Show, Eq, Num)

instance Binary Version where
  put (Version v) = put v
  get = Version <$> get

defaultVersion :: Version
defaultVersion = Version 1

-- | Specialized put/get interface to cope with different versions of
-- data structure.
class (Binary s) => Versionable s where
  -- | Write the given type `s` as bytes for a given version
  write :: Version -> s -> BS.ByteString
  write _ = toStrict . runPut . put

  -- | Read some bytes at some version as an `s`
  read :: Version -> BS.ByteString -> Either String s
  read _ = runGet get . fromStrict

instance Versionable () where
  write = undefined
  read = undefined

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

instance (Binary a) => Binary (StoredEvent a) where
  put StoredEvent {..} = do
    put $ version eventVersion
    put $ eventDate
    putByteString $ unSha1 eventSHA1
    let payload = toStrict $ runPut $ put event
        len = BS.length payload
    putWord64le (fromIntegral len)
    putByteString payload

  get = do
    v <- Version <$> get
    d <- get
    s <- SHA1 <$> getByteString 20
    l <- fromIntegral <$> getWord64le
    p <- getLazyByteString l
    case runGet get p of
      Right val -> return $ StoredEvent v d s val
      Left err -> fail err

-- | Convert a serializable to ByteString for binary storage
doStore :: (?currentVersion :: Version, Versionable s) => s -> BS.ByteString
doStore e =
  let bs = write ?currentVersion e
      crc = 42 -- TODO compute real CRC32
   in toStrict $ runPut $ do
        putWord32be $ fromIntegral (BS.length bs + 4 + 1)
        putWord8 (fromIntegral $ version ?currentVersion)
        putWord32be crc
        putByteString bs
