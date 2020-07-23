{-# LANGUAGE FlexibleInstances #-}

-- | An pure in-memory implementation of `HStore` event storage interface
--
-- This is mostly useful for testing purpose as it has obviously the lowest
-- overhead and the simplest setup requirements.
module HStore.InMemoryOps where

import Control.Concurrent.STM
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson (Result (..), Value, fromJSON, toJSON)
import Data.Either
import Data.Vector ((++), Vector, empty, fromList, slice, toList)
import HStore
import HStore.Events
import Prelude hiding ((++))

data InMemoryStorage
  = InMem
      { cells :: TVar (Vector Value),
        currentRevision :: TVar Revision
      }

newInMemoryStorage :: IO InMemoryStorage
newInMemoryStorage = InMem <$> newTVarIO empty <*> newTVarIO 0

-- | Run given action passing it an initialised `InMemoryStorage` instance
withMemoryStore ::
  (InMemoryStorage -> IO a) -> IO a
withMemoryStore act = newInMemoryStorage >>= act

instance (MonadIO m) => Store m InMemoryStorage where
  store InMem {..} _rev evs =
    let newVec = fromList $ fmap toJSON evs
     in liftIO $ atomically $ do
          oldRev <- readTVar currentRevision
          let newRev = oldRev + fromIntegral (length newVec)
          writeTVar currentRevision newRev
          oldVec <- readTVar cells
          writeTVar cells (oldVec ++ newVec)
          pure $ StoreSuccess newRev

  load InMem {..} rev off = liftIO $ atomically $ do
    vec <- readTVar cells
    curRev <- readTVar currentRevision
    if rev > curRev
      then pure $ LoadFailure (InvalidRevision rev curRev)
      else loadValues vec
    where
      loadValues vec = do
        let realOff = min (length vec - fromIntegral rev) (fromIntegral off)
            evs = toList $ slice (fromIntegral rev) (fromIntegral $ realOff) vec
            fromResult (Success a) = Right a
            fromResult (Error s) = Left s
            (errs, vals) = partitionEithers $ fmap (fromResult . fromJSON) evs
        pure $ case errs of
          [] -> LoadSuccess vals
          _ -> LoadFailure (StoreError $ "cannot decode some values " <> show (take 5 errs))

  loadAll InMem{..} initState f = liftIO $ atomically $ do
    vec <- readTVar cells
    rev <- readTVar currentRevision
    pure $ LoadSuccess (foldl impuref initState vec ,rev)
      where
        impuref a e = f a $ fromRight (error "this should not fail") (parseEvent e)
