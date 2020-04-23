{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HStore.PostgresOps.TestHelper where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import HStore.PostgresOps
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp as Warp
import System.Exit
import System.Process.Typed
import Prelude hiding (init)

withPGDatabase ::
  (PGStorageOptions -> IO c) -> IO c
withPGDatabase = bracket startPostgres stopPostgres
  where
    startPostgres = do
      (freePort, sock) <- openFreePort
      Network.close sock
      let name = "postgres-" <> show freePort
      let pgOptions = defaultOptions {dbPort = freePort}
      runProcess_ $
        proc
          "docker"
          [ "run",
            "-d",
            "-e",
            "POSTGRES_HOST_AUTH_METHOD=trust",
            "--name",
            name,
            "-p",
            show freePort <> ":5432",
            "postgres:9.6"
          ]
      (_, ip, _) <- readProcess $ proc "docker" ["inspect", "--format={{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}", name]
      ready <- waitForPostgresReady ip name 30
      if not ready
        then pure $ pgOptions
        else do
          createDatabase (pgOptions {dbUser = "postgres", dbPassword = "", dbName = "postgres"}) "hstore" "hstore" "" >>= print
          migrateDatabase pgOptions >>= print
          pure $ pgOptions
    stopPostgres PGStorageOptions {dbPort} = do
      let name = "postgres-" <> show dbPort
      runProcess_ $ proc "docker" ["rm", "-f", name]
      pure ()
    trim = filter (not . isSpace)
    waitForPostgresReady ::
      LBS.ByteString -> String -> Int -> IO Bool
    waitForPostgresReady _ _ 0 = pure False
    waitForPostgresReady ip name n =
      runProcess (proc "docker" ["exec", "-t", name, "pg_isready", "-h", trim $ unpack $ decodeUtf8 ip])
        >>= \case
          ExitSuccess -> pure True
          ExitFailure _ -> threadDelay 1000000 >> waitForPostgresReady ip name (n -1)
