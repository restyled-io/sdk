module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.Info.Resolved (RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Lint
import Restylers.Image (RestylerImage)
import Restylers.Manifest (toRestyler)
import qualified Restylers.Manifest as Manifest
import RIO.Process
import Restylers.Options
import Restylers.Test

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts
            case oCommand of
                Build noCache lint test push yaml -> do
                    info <- Info.load yaml
                    whenLintDockerfile lint $ lintRestyler info
                    image <- buildRestylerImage noCache info
                    whenRunTests test $ testRestylerImage info image
                    whenPush push $ pushRestylerImage image

                Release manifest yamls -> do
                    restylers <- for yamls $ \yaml -> do
                        logInfo $ "Verifying " <> fromString yaml
                        info <- Info.load yaml
                        image <- ensureImage info =<< getRestylerImage info
                        pure $ toRestyler info image
                    logInfo
                        $ "Writing "
                        <> displayShow (length restylers)
                        <> " Restyler(s) to "
                        <> fromString manifest
                    liftIO $ Manifest.write manifest restylers

ensureImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> Maybe RestylerImage
    -> m RestylerImage
ensureImage info = \case
    Nothing -> do
        logInfo "Image not found, building now"
        buildTestPush info
    Just image -> do
        exists <- doesRestylerImageExist image
        if exists
            then pure image
            else do
                logInfo "Image not found, building now"
                buildTestPush info

buildTestPush
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> m RestylerImage
buildTestPush info = do
    image <- buildRestylerImage (NoCache False) info
    testRestylerImage info image
    image <$ pushRestylerImage image
