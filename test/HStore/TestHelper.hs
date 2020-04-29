{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module HStore.TestHelper where

import Data.Aeson
import HStore
import Test.QuickCheck

newtype Added = Added {unadded :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Num, ToJSON, FromJSON)

instance Arbitrary Added where
  arbitrary = Added <$> choose (1, 100)

instance Versionable Added
