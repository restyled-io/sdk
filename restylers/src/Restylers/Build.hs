module Restylers.Build
    ( buildRestylerImage
    , getRestylerImage
    , doesRestylerImageExist
    , pushRestylerImage
    )
where

import RIO hiding (to)

import Restylers.Image
import qualified Restylers.Info.Build as Build
import Restylers.Info.Resolved (ImageSource(..), RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Options
import Restylers.Version
import qualified RIO.ByteString.Lazy as BSL
import RIO.Directory (doesFileExist)
import qualified RIO.Map as Map
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => NoCache
    -> RestylerInfo
    -> m RestylerImage
buildRestylerImage noCache info = do
    registry <- oRegistry <$> view optionsL
    case Info.imageSource info of
        Explicit image -> do
            logInfo $ "Pulling explicit image, " <> display image
            image <$ proc "docker" ["pull", unImage image] runProcess
        BuildVersionCmd name cmd options -> do
            tag <- oTag <$> view optionsL
            image <- Build.build noCache options
                $ mkRestylerImage registry name tag
            version <- dockerRunSh image cmd
            let versioned = mkRestylerImage registry name version
            writeFileUtf8 (Build.versionCache options) $ version <> "\n"
            logInfo $ "Tagging " <> display image <> " => " <> display versioned
            versioned <$ dockerTag image versioned
        BuildVersion name version options -> do
            Build.build noCache options
                $ mkRestylerImage registry name
                $ unRestylerVersion version

getRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasOptions env)
    => RestylerInfo
    -> m (Maybe RestylerImage)
getRestylerImage info = do
    registry <- oRegistry <$> view optionsL
    case Info.imageSource info of
        Explicit image -> pure $ Just image
        BuildVersionCmd name _ options -> do
            let cache = Build.versionCache options
            exists <- doesFileExist cache
            if exists
                then do
                    version <- T.strip <$> readFileUtf8 cache
                    pure $ Just $ mkRestylerImage registry name version
                else Nothing <$ logWarn
                    ("Unable to find image name for "
                    <> display name
                    <> ": uses version_cmd and no .version file committed"
                    )
        BuildVersion name version _ -> do
            pure $ Just $ mkRestylerImage registry name $ unRestylerVersion
                version

doesRestylerImageExist
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> m Bool
doesRestylerImageExist image =
    withModifyEnvVars (Map.insert "DOCKER_CLI_EXPERIMENTAL" "enabled") $ do
        (ec, _stdout, _stderr) <- proc
            "docker"
            ["manifest", "inspect", unImage image]
            readProcess
        pure $ ec == ExitSuccess

pushRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> m ()
pushRestylerImage image = proc "docker" ["push", unImage image] runProcess_

dockerRunSh
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> String
    -> m Text
dockerRunSh image cmd = do
    bs <- proc
        "docker"
        (concat
            [ ["run", "--rm"]
            , ["--entrypoint", "sh"]
            , [unpack $ unRestylerImage image]
            , ["-c", cmd]
            ]
        )
        readProcessStdout_
    pure $ T.strip $ decodeUtf8With lenientDecode $ BSL.toStrict bs

dockerTag
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> RestylerImage
    -> m ()
dockerTag from to = proc "docker" ["tag", unImage from, unImage to] runProcess_

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
