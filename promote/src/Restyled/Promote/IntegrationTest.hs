module Restyled.Promote.IntegrationTest
  ( TestCount (..)
  , RestyleCmd
  , optRestyleCmd
  , runIntegrationTest
  )
where

import RIO

import Control.Error.Util (note)
import Options.Applicative
import qualified RIO.ByteString.Lazy as BSL
import RIO.Directory (withCurrentDirectory)
import qualified RIO.NonEmpty as NE
import RIO.Process
import qualified RIO.Text as T
import Restyled.Promote.Channel
import Restyled.Promote.IntegrationTest.Setup
import Restyled.Promote.Manifest
import qualified ShellWords

newtype RestyleCmd = RestyleCmd
  { unRestyleCmd :: NonEmpty String
  }

instance Show RestyleCmd where
  show = unwords . toList . unRestyleCmd

defaultRestyleCmd :: RestyleCmd
defaultRestyleCmd = RestyleCmd $ pure "restyle"

readRestyleCmd :: String -> Either String RestyleCmd
readRestyleCmd s = do
  wds <- ShellWords.parse s
  cmd <- note "Restyle cmd cannot be empty" $ NE.nonEmpty wds
  pure $ RestyleCmd cmd

optRestyleCmd :: Parser RestyleCmd
optRestyleCmd =
  option
    (eitherReader readRestyleCmd)
    ( long "restyle-cmd"
        <> help ""
        <> value defaultRestyleCmd
        <> showDefault
    )

runIntegrationTest
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => Channel
  -> TestCount
  -> RestyleCmd
  -> m ()
runIntegrationTest channel tc (RestyleCmd restyleCmd) = do
  logInfo
    $ mconcat
      [ "Testing "
      , case tc of
          TestAll -> "all Restylers"
          TestOnly n -> display @Int (fromIntegral n) <> " random Restyler(s)"
      , " from the "
      , display channel
      , " manifest"
      ]

  withManifest channel $ \manifest -> do
    withSystemTempDirectory "" $ \tmp -> do
      withCurrentDirectory tmp $ do
        setupManifestTestFiles channel manifest tc
        logDebug . display =<< readFileUtf8 ".restyled.yaml"

        logInfo "Committing config and test files"
        proc "git" ["init", "."] runProcess_
        proc "git" ["add", "."] runProcess_
        proc "git" ["commit", "-m", "Add test files"] runProcess_
        sha <-
          T.unpack
            . T.dropWhileEnd (== '\n')
            . decodeUtf8With lenientDecode
            . BSL.toStrict
            <$> proc "git" ["rev-parse", "HEAD"] readProcessStdout_

        logInfo "Running restyle"
        proc
          (NE.head restyleCmd)
          ( concat
              [ NE.tail restyleCmd
              , ["--color", "always"]
              , ["--manifest", manifest]
              , ["."]
              ]
          )
          runProcess_

        logInfo "Differences patch:"
        proc "git" ["--no-pager", "format-patch", "--stdout", sha] runProcess_
