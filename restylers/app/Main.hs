module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import qualified Restylers.Info.Resolved as Info
import Restylers.Lint
import Restylers.Manifest (toRestyler)
import qualified Restylers.Manifest as Manifest
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

            restylers <- for oInputs $ \yaml -> do
                logDebug $ "Input: " <> fromString yaml
                info <- Info.load yaml
                image <- getRestylerImage info
                logInfo $ "Restyler image: " <> display image
                exists <- doesRestylerImageExist image

                if exists && not oAlways
                    then logInfo "Image exists, skipping"
                    else do
                        lintRestyler info
                        buildRestylerImage info image
                        testRestylerImage info image
                        when oPush $ pushRestylerImage image

                pure $ toRestyler info image

            traverse_ (liftIO . (`Manifest.write` restylers)) oWrite
