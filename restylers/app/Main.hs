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
                Build yaml -> do
                    logDebug $ fromString yaml
                    void $ build False =<< Info.load yaml
                Release manifest yamls -> do
                    restylers <- for yamls $ \yaml -> do
                        logDebug $ fromString yaml
                        info <- Info.load yaml
                        image <- build True info
                        pure $ toRestyler info image
                    liftIO $ Manifest.write manifest restylers

build
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => Bool -- ^ Push after build?
    -> RestylerInfo
    -> m RestylerImage
build push info = do
    image <- getRestylerImage info
    logInfo $ "Restyler image: " <> display image
    exists <- doesRestylerImageExist image

    if exists
        then logInfo "Image exists, skipping"
        else do
            lintRestyler info
            buildRestylerImage info image
            testRestylerImage info image
            when push $ pushRestylerImage image

    pure image
