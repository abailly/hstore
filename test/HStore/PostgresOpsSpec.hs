{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HStore.PostgresOpsSpec
  ( spec,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary
import Data.Either
import Data.Functor
import HStore
import HStore.PostgresOps
import HStore.PostgresOps.TestHelper
import Test.Hspec
import Test.QuickCheck as Q
import Prelude hiding (init)

newtype Added = Added {unadded :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Binary, Num, ToJSON, FromJSON)

instance Arbitrary Added where
  arbitrary = Added <$> choose (1, 100)

instance Versionable Added

spec :: Spec
spec = around withPGDatabase $ describe "Postgres Storage" $ do
  it "should write and load all to postgres store" $ \st -> do
    commands <- generate (arbitrary :: Gen [Added])
    let storeAll storage = mapM (\(e, r) -> store storage r [e]) (zip commands [0 ..])
    _res <- withPostgresStorage st $ storeAll
    evs' <- withPostgresStorage st (\s -> load s 0 (fromIntegral $ length commands))
    evs' `shouldBe` Right (LoadSuccess commands)
  it "should fail to write to postgres store given revision is invalid" $ \st -> do
    ad <- generate (arbitrary :: Gen Added)
    ad' <- generate (arbitrary :: Gen Added)
    write1 <- withPostgresStorage st $ \s -> store s 0 [ad]
    write1 `shouldBe` Right (StoreSuccess 1)
    res <- withPostgresStorage st $ \s -> store s 0 [ad']
    res `shouldBe` Right (StoreFailure $ InvalidRevision 0 1)
  it "can write muliple events at once" $ \st -> do
    events <- generate (arbitrary :: Gen [Added])
    res <- withPostgresStorage st $ \s -> store s 0 events
    res `shouldBe` Right (StoreSuccess $ fromIntegral (length events))
  it "loads requested slice of events" $ \st -> do
    events <- sequence $ replicate 20 (generate (arbitrary :: Gen Added))
    res <- withPostgresStorage st $ \s -> store s 0 events
    res `shouldBe` Right (StoreSuccess 20)
    evs <- withPostgresStorage st $ \s -> load s 0 10
    evs `shouldBe` Right (LoadSuccess $ take 10 events)
  it "loads nothing given revision is greater than current" $ \st -> do
    events <- sequence $ replicate 20 (generate (arbitrary :: Gen Added))
    res <- withPostgresStorage st $ \s -> store s 0 events
    res `shouldBe` Right (StoreSuccess 20)
    evs <- withPostgresStorage st $ \s -> load s 21 10
    evs `shouldBe` Right (LoadSuccess [] :: LoadResult [Added])
  it "loads all stored events with a callback" $ \st -> do
    events <- sequence $ replicate 200 (generate (arbitrary :: Gen Added))
    let f acc (Added x) = acc + x
    res <- withPostgresStorage st $ \s -> do
      _ <- store s 0 events
      loadAll s 0 f
    res `shouldBe` Right (LoadSuccess (unadded $ sum events, 200))
  it "loading all events updates revision" $ \st -> do
    events <- sequence $ replicate 10 (generate (arbitrary :: Gen Added))
    events' <- sequence $ replicate 10 (generate (arbitrary :: Gen Added))
    let f acc (Added x) = acc + x
    res <- withPostgresStorage st $ \s -> do
      _ <- store s 0 events
      _ <- loadAll s 0 f
      store s 0 events'
    res `shouldBe` Right (StoreFailure $ InvalidRevision 0 10)
