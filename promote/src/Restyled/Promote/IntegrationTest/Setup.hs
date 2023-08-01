{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Promote.IntegrationTest.Setup
  ( setupManifestTestFiles
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

data Restylers = Restylers
  { restylers_version :: Text
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
  -> m ()
setupManifestTestFiles channel path = do
  restylers <- Yaml.decodeFileThrow path
  createRestylersYaml channel restylers

  for_ restylers $ \Restyler {name, include, metadata = Metadata {tests}} ->
    for_ (zip [0 ..] tests) $ \(n, test) ->
      -- Skip whitespace test that can't be committed
      unless ("\r\n" `T.isInfixOf` contents test) $ do
        writeTestFiles n name include test

createRestylersYaml
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Channel
  -> [Restyler]
  -> m ()
createRestylersYaml channel restylers = do
  logInfo "CREATE .restyled.yaml"
  writeFileUtf8 ".restyled.yaml"
    $ decodeUtf8With lenientDecode
    $ Yaml.encode
    $ Restylers
      { restylers_version = channelName channel
      , restylers = map toConfigRestyler restylers
      }
