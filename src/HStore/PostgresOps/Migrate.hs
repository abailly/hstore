{-# LANGUAGE OverloadedStrings #-}

-- | Manages the PostgresSQL database to store events
--
-- This module is responsible for 2 things:
--
-- * Creating the initial structure of the DB that will store events
-- * Migrating the DB as modifications are made to the format of the
--  data storage
module HStore.PostgresOps.Migrate
  ( createDatabase,
    migrateDatabase,
  )
where

import Control.Exception.Safe (catchAny)
import Data.Functor (void)
import Data.Text (Text)
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Types
import HStore.PostgresOps.Types

createDb, createUser, grantRights :: Query
createDb = "CREATE DATABASE ?"
createUser = "CREATE USER ? WITH ENCRYPTED PASSWORD ?"
grantRights = "GRANT ALL PRIVILEGES ON DATABASE ? TO ?"

makeDb ::
  Connection -> Text -> Text -> Text -> IO ()
makeDb connection newDbName newUserName newPassword = do
  void $ execute connection createDb (Only $ Identifier newDbName)
  void $ execute connection createUser (Identifier newUserName, newPassword)
  void $ execute connection grantRights (Identifier newDbName, Identifier newUserName)

createTable :: MigrationCommand
createTable =
  MigrationScript
    "CreateTable"
    "CREATE TABLE events ( event_id SERIAL PRIMARY KEY, event_version INT, event_date TIMESTAMPTZ, event_sha1 CHAR(40), event JSON)"

migrations :: [MigrationCommand]
migrations = [MigrationInitialization, createTable]

-- | Creates (initialises) the complete DB for storing evenrs
--
--  It assumes the `dbName`, `dbUser` and `dbPassword` set in the `opts`
--  storage options are correctly set to the superuser of the database
--  server we are connecting to.
createDatabase ::
  PGStorageOptions -> Text -> Text -> Text -> IO (MigrationResult String)
createDatabase opts@PGStorageOptions {..} newDbName newUserName newPassword = do
  let connectInfo = makeConnectInfo opts
  ( do
      connection <- connect connectInfo
      makeDb connection newDbName newUserName newPassword
      pure MigrationSuccess
    )
    `catchAny` \err -> pure (MigrationError (show err))

migrateDatabase ::
  PGStorageOptions -> IO (MigrationResult String)
migrateDatabase opts = do
  let connectInfo = makeConnectInfo opts
  ( do
      connection <- connect connectInfo
      withTransaction connection $ runMigrations True connection migrations
    )
    `catchAny` \err -> pure (MigrationError (show err))
