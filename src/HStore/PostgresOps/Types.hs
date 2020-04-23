module HStore.PostgresOps.Types where

import Control.Concurrent.MVar(MVar)
import Database.PostgreSQL.Simple as PG
import HStore

-- | Internal definition of the storage to use for operations
data PostgresStorage
  = PostgresStorage
      { dbConnectInfo :: ConnectInfo,
        dbConnection :: Maybe Connection,
        dbVersion :: Version,
        dbRevision :: MVar Revision
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
