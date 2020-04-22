{-# LANGUAGE DeriveGeneric #-}

-- | Interface and types for persisting events in an underlying `Store`
module HStore
  ( module HStore.Events,
    Versionable (..),
    Version (..),
    StorageResult (..),
    Store (..),
  )
where

import HStore.Events
import Data.Aeson(ToJSON, FromJSON)

class Store m store where
  close :: store -> m store
  store ::
    (Versionable event, ToJSON event) =>
    -- | Storage Engine
    store ->
    -- | Pre-treatment action that returns something to serialize or an error that is passed down to post
    --  as is
    m (Either error event) ->
    -- | Post-treatment action that provides some result out of storage result or error in pre-treatment
    (Either error (StorageResult event) -> m result) ->
    m (StorageResult result)
  load :: (Versionable event, FromJSON event) => store -> m (StorageResult event)
  reset :: store -> m (StorageResult ())

-- | Result of storage operations.
data StorageResult s where
  OpFailed :: {failureReason :: String} -> StorageResult s
  WriteSucceed :: s -> StorageResult s
  WriteFailed :: s -> StorageResult s
  LoadSucceed :: (Versionable s) => [s] -> StorageResult s
  ResetSucceed :: StorageResult s
  NoOp :: StorageResult s

instance (Show s) => Show (StorageResult s) where
  show (OpFailed r) = "OpFailed " ++ r
  show (WriteSucceed s) = "WriteSucceed " ++ show s
  show (WriteFailed f) = "WriteSucceed " ++ show f
  show (LoadSucceed ss) = "LoadSucceed " ++ show (length ss)
  show ResetSucceed = "ResetSucceed"
  show NoOp = "NoOp"
