{-# LANGUAGE FlexibleInstances #-}

-- | An pure in-memory implementation of `HStore` event storage interface
--
-- This is mostly useful for testing purpose as it has obviously the lowest
-- overhead and the simplest setup requirements.
module HStore.InMemoryOps where

import Control.Concurrent.STM
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson (Value, toJSON,fromJSON, Result(..))
import Data.Vector (Vector, empty, toList, fromList, (++), slice)
import HStore
import Data.Either
import Debug.Trace
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
  store InMem{..} _rev evs =
    let newVec = fromList $ fmap toJSON evs
    in liftIO $ atomically $ do
      oldRev <- readTVar currentRevision
      let newRev = oldRev + fromIntegral (length newVec)
      writeTVar currentRevision newRev
      oldVec <- readTVar cells
      writeTVar cells (oldVec ++ newVec)
      pure $ StoreSuccess newRev

  load InMem{..} rev off = liftIO $ atomically $ do
    vec <- readTVar cells
    curRev <- readTVar currentRevision
    if rev > curRev
      then pure $ LoadFailure (InvalidRevision rev curRev)
      else trace ("rev:  " <> show rev <> ", cur: " <> show curRev) $ loadValues vec
      where
        loadValues vec = do
          let evs = toList $ slice (fromIntegral rev) (fromIntegral off) vec
              fromResult (Success a) = Right a
              fromResult (Error s) = Left s
              (errs, vals) = partitionEithers $ fmap (fromResult . fromJSON) evs
          pure $ case errs of
                   [] -> LoadSuccess vals
                   _ -> LoadFailure (StoreError $ "cannot decode some values " <> show (take 5 errs))

  loadAll _s _initState _f = undefined
