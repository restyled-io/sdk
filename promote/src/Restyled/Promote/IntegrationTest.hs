module Restyled.Promote.IntegrationTest
    ( IntegrationTests(..)
    , integrationTest
    , readIntegrationTest
    , IntegrationTestOptions(..)
    , runIntegrationTest
    )
where

import RIO

import Restyled.Promote.Channel
import Control.Monad.Extra (orM, andM)
import Restyled.Promote.Manifest
import qualified RIO.ByteString.Lazy as BSL
import RIO.Process
import RIO.Directory (withCurrentDirectory)
import RIO.Text (unpack, pack)
import Restyled.Promote.IntegrationTest.Setup
import qualified RIO.Text as T

data IntegrationTests
    = IntegrationTestDemo45

readIntegrationTest :: String -> Either String IntegrationTests
readIntegrationTest = \case
    "demo45" -> Right IntegrationTestDemo45
    x -> Left $ "Invalid test " <> show x

integrationTest :: IntegrationTests -> Text -> Bool -> IntegrationTestOptions
integrationTest IntegrationTestDemo45 image debug = IntegrationTestOptions
    { oitApp = "dev"
    , oitInstallationId = 58920
    , oitRepo = "restyled-io/demo"
    , oitBranch = "testing/all"
    , oitPullRequest = 45
    , oitRestylerImage = image
    , oitDebug = debug
    }

data IntegrationTestOptions = IntegrationTestOptions
    { oitApp :: Text
    , oitInstallationId :: Int
    , oitRepo :: Text
    , oitBranch :: Text
    , oitPullRequest :: Int
    , oitRestylerImage :: Text
    , oitDebug :: Bool
    }

runIntegrationTest
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => Channel
    -> IntegrationTestOptions
    -> m ()
runIntegrationTest channel IntegrationTestOptions {..} = do
    let pr = oitRepo <> "#" <> tshow oitPullRequest

    logInfo $ "Channel: " <> display channel
    logInfo $ "Restyler Image: " <> display oitRestylerImage
    logInfo $ "Pull Request: " <> display pr

    token <- getAccessToken oitApp oitInstallationId
    let
        cloneUrl =
            "https://x-access-token:"
                <> token
                <> "@github.com/"
                <> oitRepo
                <> ".git"

    withManifest channel $ \manifest -> do
        withTemporaryClone cloneUrl $ do
            void $ orM
                [ git "checkout" ["--quiet", oitBranch]
                , True <$ git_ "checkout" ["--quiet", "-b", oitBranch]
                ]

            setupManifestTestFiles channel manifest

            void $ andM
                [ git "add" ["."]
                , git "commit" ["-m", "Update test case files"]
                , True <$ git_ "push" []
                ]

    proc
        "docker"
        (concat
            [ ["run", "--interactive", "--rm"]
            , if oitDebug then ["--env", "DEBUG=1"] else []
            , ["--env", "GITHUB_ACCESS_TOKEN=" <> unpack token]
            , ["--volume", "/tmp:/tmp"]
            , ["--volume", "/var/run/docker.sock:/var/run/docker.sock"]
            , [unpack oitRestylerImage]
            , ["--job-url", "https://example.com"]
            , ["--color=always", unpack pr]
            ]
        )
        runProcess_

getAccessToken
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Text
    -> Int
    -> m Text
getAccessToken env installationId =
    T.strip
        . decodeUtf8With lenientDecode
        . BSL.toStrict
        <$> proc
                "restyled"
                ["get-access-token", unpack env, show installationId]
                readProcessStdout_

withTemporaryClone
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => Text
    -> m a
    -> m a
withTemporaryClone url f = withSystemTempDirectory "" $ \tmp -> do
    git_ "clone" [url, pack tmp]
    withCurrentDirectory tmp f

git
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Text
    -> [Text]
    -> m Bool
git cmd args = do
    ec <- proc "git" (map unpack $ cmd : args) runProcess
    pure $ ec == ExitSuccess

git_
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Text
    -> [Text]
    -> m ()
git_ cmd args = do
    success <- git cmd args
    unless success
        $ throwString
        $ "git command was not successful: "
        <> show cmd
        <> " "
        <> show args
