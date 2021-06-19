module HStore.InMemoryOpsSpec where

import HStore
import HStore.InMemoryOps
import HStore.TestHelper
import Test.Hspec
import Test.QuickCheck as Q
import Test.QuickCheck.Monadic as Q

prop_canLoadAllStoredEvents ::
  InMemoryStorage -> [Added] -> Property
prop_canLoadAllStoredEvents inMem added = monadicIO $ do
  let f acc (Added x) = acc + x
  res <- run $ do
    _ <- store inMem 0 added
    LoadSuccess res <- loadAll inMem 0 f
    pure res

  assert $ res == (unadded (sum added), fromIntegral $ length added)

spec :: Spec
spec = around withMemoryStore $
  describe "InMemory HStore" $ do
    it "should load all events written to the store" $ \s -> property (prop_canLoadAllStoredEvents s)

    it "should fail to load events if requested revision is past current revision" $ \s -> do
      ad <- generate (arbitrary :: Gen [Added])
      StoreSuccess rev <- store s 0 ad
      let offsideRev = fromIntegral $ length ad + 1
      res :: LoadResult [Added] <- load s offsideRev 12
      res `shouldBe` LoadFailure (InvalidRevision offsideRev rev)

    it "should load only part of events given requested offset is past available data" $ \s -> do
      ad <- generate (vectorOf 20 arbitrary :: Gen [Added])
      StoreSuccess _ <- store s 0 ad

      let rev = fromIntegral $ length ad - 10

      res :: LoadResult [Added] <- load s rev 20

      length <$> res `shouldBe` LoadSuccess 10
