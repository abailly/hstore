{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HStore.PostgresOpsSpec
  ( spec,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary
import Data.Either
import Data.Functor
import HStore
import HStore.PostgresOps
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp as Warp
import System.Exit
import System.Process.Typed
import Test.Hspec
import Test.QuickCheck as Q
import Prelude hiding (init)

newtype Added = Added Int
  deriving stock (Eq, Show)
  deriving newtype (Binary, ToJSON, FromJSON)

instance Arbitrary Added where
  arbitrary = Added <$> choose (1, 100)

instance Versionable Added

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
      ready <- waitForPostgresReady name 30
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
    waitForPostgresReady ::
      String -> Int -> IO Bool
    waitForPostgresReady _ 0 = pure False
    waitForPostgresReady name n =
      runProcess (proc "docker" ["exec", "-t", name, "pg_isready"])
        >>= \case
          ExitSuccess -> pure True
          ExitFailure _ -> threadDelay 1000000 >> waitForPostgresReady name (n -1)

spec :: Spec
spec = around withPGDatabase $ describe "Postgres Storage" $ do
  it "should write to postgres store" $ \st -> do
    commands <- generate (arbitrary :: Gen [Added])

    let storeAll storage = mapM (\(e,r) -> store storage r [e]) (zip commands [0..])
    _ <- withPostgresStorage st $ storeAll
    evs' <- withPostgresStorage st (\ s -> load s 0 0)
    evs' `shouldBe` Right (LoadSuccess commands)

  it "should fail to write to postgres store given revision is invalid" $ \st -> do
    ad <- generate (arbitrary :: Gen Added)
    ad' <- generate (arbitrary :: Gen Added)
    Right (StoreSuccess 1) <- withPostgresStorage st $ \s -> store s 0 [ad]
    res <- withPostgresStorage st $ \ s -> store s 0 [ad']
    res `shouldBe` Right (StoreFailure $ InvalidRevision 0 1)
