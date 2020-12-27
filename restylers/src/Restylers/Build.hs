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
import qualified RIO.Map as Map
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

buildRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> RestylerImage
    -> m ()
buildRestylerImage info image = case Info.imageSource info of
    Explicit x -> logInfo $ "Not bulding explicit image, " <> display x
    BuildVersionCmd _name _cmd options -> do
        logInfo $ "Building " <> display image
        void $ Build.build options image
    BuildVersion _name _version options -> do
        logInfo $ "Building " <> display image
        void $ Build.build options image

getRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> m RestylerImage
getRestylerImage info = do
    registry <- oRegistry <$> view optionsL

    case Info.imageSource info of
        Explicit image -> pure image
        BuildVersionCmd name cmd options -> do
            logInfo $ "Building " <> display name <> " for version_cmd"
            tag <- oTag <$> view optionsL
            image <- Build.build options $ mkRestylerImage registry name tag
            version <- dockerRunSh image cmd
            let versioned = mkRestylerImage registry name version
            versioned <$ dockerTag image versioned
        BuildVersion name explicitVersion _ -> do
            let version = unRestylerVersion explicitVersion
            pure $ mkRestylerImage registry name version

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
pushRestylerImage image = do
    logInfo $ "Pushing " <> display image
    proc "docker" ["push", unImage image] runProcess_

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
