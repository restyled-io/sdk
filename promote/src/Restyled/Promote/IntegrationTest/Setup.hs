{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Promote.IntegrationTest.Setup
  ( setupManifestTestFiles
  ) where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath (takeDirectory, (<.>), (</>))
import RIO.List (headMaybe)
import qualified RIO.Map as Map
import RIO.Text (unpack)
import qualified RIO.Text as T
import Restyled.Promote.Channel

data Restyler = Restyler
  { name :: Text
  , include :: [Text]
  , metadata :: Maybe Metadata
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Metadata = Metadata
  { tests :: Maybe [TestCase]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data TestCase = TestCase
  { support :: Maybe SupportFile
  , extension :: Maybe Text
  , contents :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

testCaseFiles
  :: Text
  -- ^ Restyler name
  -> Maybe Text
  -- ^ Inferred extension
  -> Int
  -- ^ Index
  -> TestCase
  -> [(FilePath, Text)]
testCaseFiles name inferredExtension n TestCase {support, extension, contents}
  | "\r\n" `T.isInfixOf` contents =
      []
  | otherwise =
      maybeToList (supportFile <$> support)
        <> [
             ( unpack name </> baseName
             , contents
             )
           ]
 where
  baseName = "test-file-" <> show @Int n <.> unpack finalExtension
  finalExtension = fromMaybe "example" $ extension <|> inferredExtension

data SupportFile = SupportFile
  { path :: FilePath
  , contents :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

supportFile :: SupportFile -> (FilePath, Text)
supportFile SupportFile {path, contents} = (path, contents)

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
  ConfigRestyler {name, enabled = True, include = name <> "/**/*"}

setupManifestTestFiles
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Channel
  -> FilePath
  -> m ()
setupManifestTestFiles channel =
  writeFiles . toFiles channel <=< Yaml.decodeFileThrow

toFiles :: Channel -> [Restyler] -> Map FilePath Text
toFiles channel restylers =
  Map.fromList
    $ (".restyled.yaml", restylersYaml)
    : concatMap toTestFiles restylers
 where
  restylersYaml =
    decodeUtf8With lenientDecode
      $ Yaml.encode
      $ Restylers
        { restylers_version = channelName channel
        , restylers = map toConfigRestyler restylers
        }

toTestFiles :: Restyler -> [(FilePath, Text)]
toTestFiles Restyler {name, include, metadata} = fromMaybe [] $ do
  Metadata {tests} <- metadata

  let mExtension =
        headMaybe
          . filter ("." `T.isInfixOf`)
          . filter (not . ("!" `T.isPrefixOf`))
          $ include

  concat . zipWith (testCaseFiles name mExtension) [0 ..] <$> tests

writeFiles
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Map FilePath Text
  -> m ()
writeFiles files = do
  -- directories <-
  --     filterM doesDirectoryExist
  --     $ filter (/= ".")
  --     $ map takeDirectory
  --     $ Map.keys files
  -- traverse_ removeDirectoryRecursive directories

  for_ (Map.toList files) $ \(path, contents) -> do
    logInfo $ "CREATE " <> fromString path
    createDirectoryIfMissing True $ takeDirectory path
    writeFileUtf8 path contents
