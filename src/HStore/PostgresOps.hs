{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Low-level file storage engine
module HStore.PostgresOps
  ( PGStorageOptions (..),
    openPostgresStorage,
    closePostgresStorage,
    withPostgresStorage,
    defaultOptions,
    createDatabase,
    migrateDatabase,
  )
where

import Control.Exception.Safe (MonadCatch, bracket, catchAny)
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), ToJSON (..), Value, fromJSON)
import qualified Data.ByteString.Base16 as Hex
import Data.Either
import Data.Functor (void)
import Data.Time.Clock
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import HStore
import HStore.PostgresOps.Migrate
import HStore.PostgresOps.Types
import Data.Word(Word64)
import System.IO
import Prelude hiding (length, read)
import qualified Data.ByteString as BS

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

data PGStorageError = PGStorageError {_reason :: String}
  deriving (Eq, Show)

openPostgresStorage :: PGStorageOptions -> IO (Either PGStorageError PostgresStorage)
openPostgresStorage opts@PGStorageOptions {storageVersion = dbVersion} = do
  let dbConnectInfo = makeConnectInfo opts
  ( do
      dbConnection <- Just <$> connect dbConnectInfo
      pure $ Right $ PostgresStorage {..}
    )
    `catchAny` \ex -> pure (Left $ PGStorageError $ show ex)

closePostgresStorage ::
  MonadIO m =>
  PostgresStorage ->
  m PostgresStorage
closePostgresStorage s@(PostgresStorage _ Nothing _) = pure s
closePostgresStorage s@(PostgresStorage _ (Just conn) _) =
  liftIO (PG.close conn) >> pure s {dbConnection = Nothing}

withPostgresStorage ::
  PGStorageOptions -> (PostgresStorage -> IO a) -> IO (Either PGStorageError a)
withPostgresStorage opts act =
  bracket (openPostgresStorage opts) closeDB $
    \case
      Left err -> pure $ Left err
      Right pg -> Right <$> act pg
  where
    closeDB (Left _) = pure ()
    closeDB (Right st) = void $ closePostgresStorage st

mkEvent ::
  (Versionable e, MonadIO m) =>
  Version ->
  e ->
  m (StoredEvent e)
mkEvent v e =
  do
    ts <- liftIO getCurrentTime
    pure $ StoredEvent v ts defaultSha1 e

instance ToField Version where
  toField (Version v) = toField v

instance FromField Version where
  fromField a b = Version . fromInteger <$> fromField a b

instance ToField SHA1 where
  toField (SHA1 bs) = toField (Hex.encode bs)

instance FromField SHA1 where
  fromField a b = SHA1 . fst . Hex.decode <$> fromField a b

instance (ToJSON e) => ToRow (StoredEvent e) where
  toRow StoredEvent {..} = toRow (eventVersion, eventDate, eventSHA1, toJSON event)

instance FromRow (StoredEvent Value) where
  fromRow =
    StoredEvent <$> field <*> field <*> field <*> field

instance FromField Word64 where
  fromField a b = fromInteger <$> fromField a b

instance FromRow Revision where
  fromRow = Revision <$> field

-- | Returns the serial value written
-- https://www.postgresql.org/docs/9.4/dml-returning.html
insertEvent :: Query
insertEvent =
  "INSERT INTO events VALUES (DEFAULT, ?, ?, ?, ?) RETURNING event_id"

writeToDB ::
  (Versionable e, MonadIO m, MonadCatch m) =>
  PostgresStorage -> Revision -> [e] ->  m StoreResult
writeToDB PostgresStorage {dbConnection = Nothing} _ _ = pure $ StoreFailure $ StoreError "no database connection"
writeToDB PostgresStorage {dbConnection = Just connection, dbVersion} _ [e] =
  (do
      rev <- mkEvent dbVersion e >>= liftIO . query connection insertEvent
      case rev of
        r:_ -> pure $ StoreSuccess r
        [] -> pure $ StoreFailure $ StoreError "insert returned no result, something's wrong"
  )
  `catchAny` \ex -> pure (StoreFailure $ StoreError $ show ex)
writeToDB _ _ _ = pure $ StoreFailure $ StoreError "don't know how to write multiple values"

selectAllEvents :: Query
selectAllEvents = "SELECT event_version, event_date, event_sha1, event FROM events"

parseEvent ::
  (FromJSON e) => Value -> Either String e
parseEvent val =
  case fromJSON val of
    Success e -> Right e
    Error e -> Left e

readFromDB ::
  (Versionable s, MonadIO m, MonadCatch m, FromJSON s) =>
  PostgresStorage -> Revision -> Word64 -> m (LoadResult s)
readFromDB PostgresStorage {dbConnection = Nothing} _ _ = pure $ LoadFailure $ StoreError $ "no database connection"
readFromDB PostgresStorage {dbConnection = Just connection} _ _ =
  ( do
      evs <- fmap (parseEvent . event) <$> liftIO (query_ connection selectAllEvents)
      case partitionEithers evs of
        ([], res) -> pure $ LoadSuccess res
        (errs, _) -> pure $ LoadFailure $ StoreError $ "failed to convert some results " <> show (take 5 errs)
  )
    `catchAny` \ex -> pure (LoadFailure $ StoreError $ show ex)

instance (MonadIO m, MonadCatch m) => Store m PostgresStorage where
  store = writeToDB
  load = readFromDB
