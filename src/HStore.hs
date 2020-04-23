{-# LANGUAGE DeriveGeneric #-}

-- | Interface and types for persisting events in an underlying `Store`
module HStore where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Aeson.Types (parseEither)
import Data.Word

-- * Events

-- | A `Revision` is the unique index of an event within the overall
-- stream of events, hinting at the fact its a /revision/ number of
-- some potential state.
newtype Revision = Revision {revision :: Word64}
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral)

-- | A `Version` is attached to each /event/ and denotes changes in
-- the structure of the event. An event stream can store events with
-- different version numbers but `Version` numbers are expected to be
-- monotonically increasing, so it should be the case that
--
-- @@
-- version e > version e' => revision e > revision e'
-- @@
newtype Version = Version {version :: Word64}
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral)

defaultVersion :: Version
defaultVersion = Version 1

-- | Events stored are expected to be `Versionable` meaning they can
-- be converted to/from JSON at a given `Version`  number. This class
-- wraps the `ToJSON`/`FromJSON` classes from /aeson/ package, adding
-- a `Version` parameter to the serialization operations.
class (ToJSON s, FromJSON s) => Versionable s where
  -- | Write the given type `s` as JSON for a given version
  -- Defaults to ignoring the @version@ argument.
  write :: Version -> s -> Value
  write _ = toJSON

  -- | Read some JSON at some `version` as an `s`
  -- Defaults to ignoring `version` argument.
  -- Returns either the parsed object or a `String` providing hopefully
  -- some explanation on the error.
  read :: Version -> Value -> Either String s
  read _ = parseEither parseJSON

-- * Storage

class Store m store where
  -- | Store a sequence of events in the underlying data stream at
  -- given `Revision` number.
  store ::
    (Versionable event) =>
    -- | Storage Engine
    store ->
    -- | The expected /revision index/ of this store
    Revision ->
    -- | The sequence of `event`s to store
    [event] ->
    -- | Operation result, either a success with the last revision number
    -- or an error
    m StoreResult

  -- | Load a sequence of events from the underlying storage
  load ::
    (Versionable event) =>
    -- | Storage engine
    store ->
    -- | The /revision/ at which to start loading data
    Revision ->
    -- | The maximum number of events to load. The operation can return less
    -- events if fewer are available from the underlying storage
    Word64 ->
    -- | Operation's result, either a success with a list of events or an
    -- error
    m (LoadResult [event])

  -- | "Streaming" interface to load all events from underlying storage in
  -- a possibly more efficient way
  loadAll ::
    (Versionable event) =>
    -- | Storage engine
    store ->
    -- | Initial state
    a ->
    -- | Callback function that will receive all events in sequence
    (a -> event -> a) ->
    -- | Final result: the final state accumulated along with the last `Revision`
    -- stored
    m (LoadResult (a, Revision))

data StoreError
  = StoreError {errorReason :: String}
  | InvalidRevision {requested :: Revision, actual :: Revision}
  deriving (Eq, Show)

data StoreResult
  = StoreSuccess {lastRevision :: Revision}
  | StoreFailure StoreError
  deriving (Eq, Show)

data LoadResult result
  = LoadSuccess result
  | LoadFailure StoreError
  deriving (Eq, Show)
