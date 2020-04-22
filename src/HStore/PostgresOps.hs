{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.Aeson (ToJSON (..))
import Data.Either
import qualified Data.ByteString.Base16 as Hex
import Data.Functor (void)
import Data.Time.Clock
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import HStore
import HStore.PostgresOps.Migrate
import HStore.PostgresOps.Types
import System.IO
import Prelude hiding (length, read)

data PGStorageError = PGStorageError {_reason :: String}

openPostgresStorage :: PGStorageOptions -> IO (Either PGStorageError PostgresStorage)
openPostgresStorage opts@PGStorageOptions{storageVersion=dbVersion} = do
  let dbConnectInfo = makeConnectInfo opts
  (do
      dbConnection <- Just <$> connect dbConnectInfo
      pure $ Right $ PostgresStorage {..})
     `catchAny` \ ex -> pure (Left $ PGStorageError $ show ex)

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
  Version -> e ->
  m (StoredEvent e)
mkEvent v e =
  do
    ts <- liftIO getCurrentTime
    pure $ StoredEvent v ts defaultSha1 e

instance ToField Version where
  toField (Version v) = toField v

instance ToField SHA1 where
  toField (SHA1 bs) = toField (Hex.encode bs)

instance (ToJSON e) => ToRow (StoredEvent e) where
  toRow StoredEvent{..} = toRow (eventVersion, eventDate, eventSHA1, toJSON event)

insertEvent :: Query
insertEvent =
  "INSERT INTO events VALUE (DEFAULT, ?, ?, ?, ?)"

writeToDB ::
  (Versionable e, ToJSON e, MonadIO m, MonadCatch m) =>
  PostgresStorage ->
  m (Either a e) ->
  (Either a (StorageResult e) -> m r) ->
  m (StorageResult r)
writeToDB PostgresStorage {dbConnection=Nothing} _pre _post = pure $ OpFailed "no database connection"
writeToDB PostgresStorage {dbConnection=Just connection, dbVersion} pre post =
  do
    p <- pre
    case p of
      Left l -> WriteFailed <$> post (Left l)
      Right payload -> do
        res <- ( do
                   _ <- mkEvent dbVersion payload >>= liftIO . execute connection insertEvent
                   pure $ WriteSucceed payload
                 )
          `catchAny` \ex -> pure (OpFailed $ show ex)
        WriteSucceed <$> post (Right res)

readFromDB ::
  (Versionable s, MonadIO m) => PostgresStorage -> m (StorageResult s)
readFromDB = undefined

resetDB ::
  MonadIO m =>
  PostgresStorage ->
  m (StorageResult ())
resetDB = const $ pure $ WriteSucceed ()

instance (MonadIO m, MonadCatch m) => Store m PostgresStorage where
  close = closePostgresStorage
  store = writeToDB
  load = readFromDB
  reset = resetDB
