{-# LANGUAGE DerivingVia #-}

module Restylers.Test
  ( testRestylerImage
  ) where

import RIO

-- import qualified RIO.ByteString.Lazy as LBS
import RIO.FilePath (takeBaseName, (</>))
import RIO.List (nub)
import RIO.Process
import RIO.Text (pack, unpack)
import Restylers.Directory
import Restylers.Image
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Info.Resolved (RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Info.Test
  ( Test
  , testFilePath
  , writeTestFiles
  )
import qualified Restylers.Info.Test as Test
import Restylers.Name (RestylerName (..))
import System.Environment (withArgs)
import Test.Hspec

testRestylerImage
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => RestylerInfo
  -> RestylerImage
  -> [String]
  -> m ()
testRestylerImage info image hspecArgs = do
  chd <- getCurrentHostDirectory
  cwd <- getCurrentDirectory

  withTempDirectory cwd "restylers-test" $ \tmp ->
    withCurrentDirectory tmp $ do
      for_ tests $ \(number, test) -> do
        writeTestFiles number (Info.name info) test

      let code = chd </> takeBaseName tmp

      withRunInIO $ \runInIO -> do
        withArgs hspecArgs $ hspec $ do
          describe (unpack $ unRestylerName $ Info.name info) $ do
            if Info.supports_multiple_paths info
              then dockerRunSpec runInIO code info image tests
              else traverse_ (dockerRunSpec runInIO code info image . pure) tests
 where
  tests = zip [1 ..] $ Metadata.tests $ Info.metadata info

dockerRunSpec
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => (forall a. m a -> IO a)
  -> FilePath
  -> RestylerInfo
  -> RestylerImage
  -> [(Int, Test)]
  -> Spec
dockerRunSpec runInIO code info image tests = do
  context testContext $ do
    delayedException <-
      runIO $
        runInIO $
          tryAny $
            runRestyler code info image tests

    for_ tests $ \(number, test) -> do
      it (testDescription number test) $ do
        -- If docker-run failed, re-throw it here so it's handled
        void $ either throwIO pure delayedException
        readFileUtf8 (testFilePath number (Info.name info) test)
          `shouldReturn` Test.restyled test
 where
  testContext :: String
  testContext =
    "docker run "
      <> unpack (unRestylerImage image)
      <> " {"
      <> case length tests of
        1 -> "1 path"
        n -> show n <> " paths"
      <> "}"

  testDescription :: Int -> Test -> String
  testDescription number test =
    unpack $
      fromMaybe ("Test #" <> pack (show number)) $
        Test.name test

runRestyler
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => FilePath
  -> RestylerInfo
  -> RestylerImage
  -> [(Int, Test)]
  -> m ()
runRestyler code info image tests = do
  void $
    proc
      "docker"
      ( nub $
          concat
            [ ["run", "--interactive", "--rm"]
            , ["--net", "none"]
            , ["--volume", code <> ":/code"]
            , [unpack $ unRestylerImage image]
            , map unpack $ Info.command info
            , map unpack $ Info.arguments info
            , ["--" | Info.supports_arg_sep info]
            , map (uncurry relativePath) tests
            ]
      )
      readProcess_
 where
  -- Restyler prepends ./, so we do too
  relativePath number test = "./" <> testFilePath number (Info.name info) test
