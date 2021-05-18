module Restylers.Lint
    ( lintRestyler
    ) where

import RIO

import Data.Aeson
import RIO.Directory (getCurrentDirectory)
import RIO.FilePath ((</>))
import RIO.Process
import qualified RIO.Text as T
import qualified Restylers.Info.Build as Build
import Restylers.Info.Resolved (ImageSource(..), RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import System.Environment (lookupEnv)

data LintError = LintError
    { code :: Text
    , message :: Text
    , level :: Text
    , file :: FilePath
    , line :: Natural
    , column :: Natural
    }
    deriving stock Generic
    deriving anyclass FromJSON

instance Display LintError where
    display LintError {..} =
        displayShow line
            <> ":"
            <> displayShow column
            <> " ["
            <> display level
            <> "] "
            <> display code
            <> ": "
            <> display message
            <> maybe "" (("\nSee " <>) . display) (wiki code)

wiki :: Text -> Maybe Text
wiki code
    | "DL" `T.isPrefixOf` code
    = Just $ "https://github.com/hadolint/hadolint/wiki/" <> code
    | "SC" `T.isPrefixOf` code
    = Just $ "https://github.com/hadolint/hadolint/wiki/" <> code
    | otherwise
    = Nothing

lintRestyler
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> m Bool
lintRestyler info = do
    case Info.imageSource info of
        Explicit{} -> False <$ logWarn "Not linting explicit image"
        BuildVersionCmd _ _ options ->
            lintDockerfile $ Build.dockerfile options
        BuildVersion _ _ options -> lintDockerfile $ Build.dockerfile options

lintDockerfile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => FilePath
    -> m Bool
lintDockerfile dockerfile = do
    logInfo $ "Linting " <> fromString dockerfile

    -- On CI, we are docker-within-docker, which means our PWD is "/code", but
    -- we need to mount these files using the non-containerized PWD, which we
    -- can (only) figure out if we're told (e.g. via ENV)
    cwd <- maybe getCurrentDirectory pure =<< liftIO (lookupEnv "REALPWD")

    bs <- proc
        "docker"
        (concat
            [ ["run", "--rm"]
            , ["--volume", cwd </> ".hadolint.yaml:/config.yaml:ro"]
            , ["--volume", cwd </> dockerfile <> ":/Dockerfile:ro"]
            , ["hadolint/hadolint", "hadolint"]
            , ["--config", "/config.yaml"]
            , ["--format", "json"]
            , ["/Dockerfile"]
            ]
        )
        readProcessStdout_

    logDebug $ "hadolint stdout: " <> displayShow bs

    case eitherDecode bs of
        Left err -> throwString $ "Unable to read hadolint output: " <> err
        Right [] -> pure False
        Right (errors :: [LintError]) -> do
            logError $ "Lint errors found in " <> fromString dockerfile
            True <$ traverse_ (logError . display) errors
