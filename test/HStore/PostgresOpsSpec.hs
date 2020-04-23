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
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Functor
import Data.Text.Lazy(unpack)
import Data.Text.Lazy.Encoding(decodeUtf8)
import Data.Char(isSpace)
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
      (_,ip,_) <- readProcess $ proc "docker" [ "inspect", "--format={{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}", name]
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

spec :: Spec
spec = around withPGDatabase $ describe "Postgres Storage" $ do
  it "should write to postgres store" $ \st -> do
    commands <- generate (arbitrary :: Gen [Added])

    let storeAll storage = mapM (\(e,r) -> store storage r [e]) (zip commands [0..])
    _res <- withPostgresStorage st $ storeAll
    evs' <- withPostgresStorage st (\ s -> load s 0 (fromIntegral $ length commands))
    evs' `shouldBe` Right (LoadSuccess commands)

  it "should fail to write to postgres store given revision is invalid" $ \st -> do
    ad <- generate (arbitrary :: Gen Added)
    ad' <- generate (arbitrary :: Gen Added)
    write1 <- withPostgresStorage st $ \s -> store s 0 [ad]
    write1 `shouldBe` Right (StoreSuccess 1)
    res <- withPostgresStorage st $ \ s -> store s 0 [ad']
    res `shouldBe` Right (StoreFailure $ InvalidRevision 0 1)

  it "can write muliple events at once" $ \st -> do
    events <- generate (arbitrary :: Gen [Added])
    res <- withPostgresStorage st $ \s -> store s 0 events
    res `shouldBe` Right (StoreSuccess $ fromIntegral (length events))

  it "loads requested slice of events" $ \ st -> do
    events <- sequence $ replicate 20 (generate (arbitrary :: Gen Added))
    res <- withPostgresStorage st $ \s -> store s 0 events
    res `shouldBe` Right (StoreSuccess 20)
    evs <- withPostgresStorage st $ \s -> load s 0 10
    evs `shouldBe` Right (LoadSuccess $ take 10 events)
