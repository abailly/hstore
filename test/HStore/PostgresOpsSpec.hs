{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad.Trans (MonadIO)
import Data.Either
import Data.Either
import Data.Functor
import Data.IORef
import Data.Serialize
import Data.Text (Text, pack)
import GHC.Generics
import HStore
import HStore.Events
import HStore.PostgresOps
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp as Warp
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix.Temp (mkstemp)
import System.Process (callCommand)
import System.Process.Typed
import Test.Hspec
import Test.QuickCheck as Q
import Test.QuickCheck.Monadic as Q
import Prelude hiding (init)

newtype Add = Add Int
  deriving (Eq, Show)

instance Arbitrary Add where
  arbitrary = Add <$> choose (1, 100)

data Added = Added Int
  deriving (Eq, Show, Generic, Serialize, Versionable)

mkEvent ::
  Applicative m => Add -> m (Either () Added)
mkEvent (Add i) = pure $ Right (Added i)

data Stored
  = Stored Added
  | Failed Text
  deriving (Eq, Show)

mkResult ::
  Applicative m => Either () (StorageResult Added) -> m Stored
mkResult (Right (WriteSucceed a)) = pure $ Stored a
mkResult (Right err) = pure $ Failed (pack $ show err)
mkResult (Left err) = pure $ Failed (pack $ show err)

storeAdd ::
  (Store m s, MonadIO m) => s -> Add -> m (Either Text Added)
storeAdd st a =
  store st (mkEvent a) mkResult
    >>= \case
      WriteSucceed (Stored a) -> pure $ Right a
      err -> pure $ Left $ pack $ show err

prop_persistentStateSerializesConcurrentWrites :: PGStorageOptions -> [[Add]] -> Property
prop_persistentStateSerializesConcurrentWrites storageOpts commands = collect (length commands) $ monadicIO $ do
  Right (errs, evs) <- Q.run $ withPostgresStorage storageOpts $ \st -> do
    void $ reset st
    evs <- partitionEithers . concat <$> mapConcurrently (mapM (storeAdd st)) commands
    return evs
  monitor (counterexample $ show commands)
  assert $ length evs == length (concat commands)
  Right (LoadSucceed evs') <- Q.run $ withPostgresStorage storageOpts load
  -- We check all events returned from actions are stored but they may be in different orders
  -- although all command execution and writes are serialized, it is possible the events be
  -- returned to this test thread in different orders
  assert $ all (`elem` evs') evs && all (`elem` evs) evs'

temporaryStorage = bracket startPostgres stopPostgres
  where
    startPostgres = do
      (freePort, sock) <- openFreePort
      Network.close sock
      let name = "postgres-" <> show freePort
      let pgOptions = defaultOptions {dbPort = freePort}
      callCommand $ "docker run -d  -e POSTGRES_HOST_AUTH_METHOD=trust --name " <> name <> " -p " <> show freePort <> ":5432 postgres:9.6 postgres -c log_statement=all"
      ready <- waitForPostgresReady name 30
      if not ready
        then pure $ pgOptions
        else do
          createDatabase (pgOptions {dbUser = "postgres", dbPassword = "", dbName = "postgres"}) "hstore" "hstore" ""
          migrateDatabase pgOptions >>= print
          pure $ pgOptions
    stopPostgres PGStorageOptions {dbPort} = do
      let name = "postgres-" <> show dbPort
      callCommand $ "docker kill " <> name
      pure ()
    waitForPostgresReady name 0 = pure False
    waitForPostgresReady name n =
      runProcess (proc "docker" ["exec", "-t", name, "pg_isready"])
        >>= \case
          ExitSuccess -> pure True
          ExitFailure _ -> threadDelay 1000000 >> waitForPostgresReady name (n -1)

spec :: Spec
spec = around temporaryStorage $ describe "Postgres Storage" $ do
  it "should serialize concurrent writes to postgres store" $
    \storageOpts -> property (prop_persistentStateSerializesConcurrentWrites storageOpts)
