{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Promote.IntegrationTest.Setup
  ( TestCount (..)
  , setupManifestTestFiles
  ) where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import qualified RIO.Text as T
import Restyled.Promote.Channel
import Restylers.Info.Metadata
import Restylers.Info.Test
import Restylers.Manifest
import Restylers.Name
import System.Random.Shuffle (shuffleM)

data TestCount
  = TestAll
  | TestOnly Natural
  deriving stock (Eq)

data RestyledYaml = RestyledYaml
  { also_exclude :: [Text]
  , restylers_version :: Text
  , restylers :: [ConfigRestyler]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ConfigRestyler = ConfigRestyler
  { name :: Text
  , enabled :: Bool
  , include :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

toConfigRestyler :: Restyler -> ConfigRestyler
toConfigRestyler Restyler {name} =
  -- include matches writeTestFiles: ./{name}-test-{n}.{extension}
  ConfigRestyler
    { name = unRestylerName name
    , enabled = True
    , include = unRestylerName name <> "-test-*"
    }

setupManifestTestFiles
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Channel
  -> FilePath
  -> TestCount
  -> m ()
setupManifestTestFiles channel path tc = do
  case tc of
    TestAll -> logInfo "Running all restyler tests"
    TestOnly n ->
      logInfo
        $ "Running tests for "
        <> display @Int (fromIntegral n)
        <> " random Restyler(s)"

  restylers <- limitByTestCount tc =<< Yaml.decodeFileThrow path
  createRestyledYaml channel restylers

  for_ restylers $ \Restyler {name, include, metadata = Metadata {tests}} ->
    for_ (zip [0 ..] tests) $ \(n, test) ->
      -- Skip whitespace test that can't be committed
      unless ("\r\n" `T.isInfixOf` contents test) $ do
        writeTestFiles n name include test

limitByTestCount :: MonadIO m => TestCount -> [a] -> m [a]
limitByTestCount tc as = liftIO $ do
  case tc of
    TestAll -> pure as
    TestOnly n -> take (fromIntegral n) <$> shuffleM as

createRestyledYaml
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Channel
  -> [Restyler]
  -> m ()
createRestyledYaml channel restylers = do
  logInfo "CREATE .restyled.yaml"
  writeFileUtf8 ".restyled.yaml"
    $ decodeUtf8With lenientDecode
    $ Yaml.encode
    $ RestyledYaml
      { also_exclude = ["./.git/**/*"]
      , restylers_version = channelName channel
      , restylers = map toConfigRestyler restylers
      }
