module HStore.InMemoryOpsSpec where

import HStore
import HStore.InMemoryOps
import HStore.TestHelper
import Test.Hspec
import Test.QuickCheck as Q
import Test.QuickCheck.Monadic as Q


prop_canLoadAllStoredEvents ::
  InMemoryStorage -> [Added] -> Property
prop_canLoadAllStoredEvents inMem added =  monadicIO $ do
  res <- run $ do
    _ <- store inMem 0 added
    LoadSuccess res <- load inMem 0 (fromIntegral $ length added)
    pure res

  assert $ res == added

spec :: Spec
spec = around withMemoryStore $ describe "InMemory HStore" $ do
  it "should load all events written to the store" $ \s -> property (prop_canLoadAllStoredEvents s)
