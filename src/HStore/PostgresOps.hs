{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | Low-level file storage engine
module HStore.PostgresOps where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (Exception, IOException, bracket, catch)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans (MonadIO (..))
import qualified Data.Binary.Get as Bin
import Data.ByteString (hGet, hPut)
import Data.ByteString.Lazy (fromStrict)
import Data.Either
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Serialize
import Data.Typeable
import Database.PostgreSQL.Simple as PG
import GHC.Natural
import HStore
import System.IO
import Prelude hiding (length, read)

-- | Internal definition of the storage to use for operations
data PostgresStorage
  = PostgresStorage
      { dbConnectInfo :: ConnectInfo,
        dbConnection :: Maybe Connection
      }

-- | Options for opening storage
data PGStorageOptions
  = PGStorageOptions
      { -- | Database name (eg. schema namespace)
        dbName :: String,
        -- | User name
        dbUser :: String,
        -- | Password for connecting to the DB
        dbPassword :: String,
        -- | Host to connect to
        dbHost :: String,
        -- | Port to connect to
        dbPort :: Int,
        -- | Events version. Note this version represents the point of view of the consumer
        --  of this storage which may contain events with different versions. The implementation
        --  of `Versionable` should be able to handle any version, up to `storageVersion`
        storageVersion :: Version
      }

defaultOptions :: PGStorageOptions
defaultOptions = PGStorageOptions "hstore" "hstore" "" "localhost" 5432 (Version 1)

makeConnectInfo ::
  PGStorageOptions -> ConnectInfo
makeConnectInfo PGStorageOptions {..} =
  ConnectInfo dbHost (fromIntegral dbPort) dbUser dbPassword dbName

data PGStorageError = PGStorageError {reason :: String}

openPostgresStorage :: PGStorageOptions -> IO (Either PGStorageError PostgresStorage)
openPostgresStorage opts = do
  let dbConnectInfo = makeConnectInfo opts
  dbConnection <- Just <$> connect dbConnectInfo
  pure $ Right $ PostgresStorage {..}

closePostgresStorage ::
  MonadIO m =>
  PostgresStorage -> m PostgresStorage
closePostgresStorage s@(PostgresStorage _ Nothing) = pure s
closePostgresStorage s@(PostgresStorage _ (Just conn)) =
  liftIO (PG.close conn) >> pure s {dbConnection = Nothing}

withPostgresStorage ::
  PGStorageOptions -> (PostgresStorage -> IO a) -> IO (Either PGStorageError a)
withPostgresStorage opts act =
  bracket (openPostgresStorage opts) closeDB $
    \case
      Left err -> pure $ Left err
      Right pg -> Right <$> act pg
  where
    closeDB (Left err) = pure ()
    closeDB (Right st) = void $ closePostgresStorage st

writeToDB ::
  (Versionable e, MonadIO m) => PostgresStorage -> m (Either a e) -> (Either a (StorageResult e) -> m r) -> m (StorageResult r)
writeToDB _storage _pre _post = undefined

readFromDB ::
  (Versionable s, MonadIO m) => PostgresStorage -> m (StorageResult s)
readFromDB = undefined

resetDB ::
  MonadIO m =>
  PostgresStorage -> m (StorageResult ())
resetDB = undefined

instance (MonadIO m) => Store m PostgresStorage where
  close = closePostgresStorage
  store = writeToDB
  load = readFromDB
  reset = resetDB
