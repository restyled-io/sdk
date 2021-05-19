module Restylers.Lint
    ( lintRestyler
    ) where

import RIO

import Data.Aeson
import RIO.FilePath ((</>))
import RIO.Process
import qualified RIO.Text as T
import Restylers.Directory
import qualified Restylers.Info.Build as Build
import Restylers.Info.Resolved (ImageSource(..), RestylerInfo)
import qualified Restylers.Info.Resolved as Info

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

    chd <- getCurrentHostDirectory

    (_, bs) <- proc
        "docker"
        (concat
            [ ["run", "--rm"]
            , ["--volume", chd </> ".hadolint.yaml:/config.yaml:ro"]
            , ["--volume", chd </> dockerfile <> ":/Dockerfile:ro"]
            , ["hadolint/hadolint", "hadolint"]
            , ["--config", "/config.yaml"]
            , ["--format", "json"]
            , ["/Dockerfile"]
            ]
        )
        readProcessStdout

    logDebug $ "hadolint stdout: " <> displayShow bs

    case eitherDecode bs of
        Left err -> throwString $ "Unable to read hadolint output: " <> err
        Right [] -> pure False
        Right (errors :: [LintError]) -> do
            logError $ "Lint errors found in " <> fromString dockerfile
            True <$ traverse_ (logError . display) errors
