module Main
    ( main
    ) where

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

            when (not oTest && (oPush || isJust oWrite))
                $ logWarn "--push and --write do nothing without --test"

            when oLint $ do
                results <- for oInputs $ \yaml -> do
                    logDebug $ "Input: " <> fromString yaml
                    lintRestyler =<< Info.load yaml

                when (or results) $ do
                    logError "Exiting due to Lint errors"
                    exitFailure

            when oBuild $ for_ oInputs $ \yaml -> do
                logDebug $ "Input: " <> fromString yaml
                info <- Info.load yaml
                buildRestylerImage info

            when oTest $ do
                restylers <- for oInputs $ \yaml -> do
                    logDebug $ "Input: " <> fromString yaml
                    info <- Info.load yaml
                    image <- tagRestylerImage info
                    testRestylerImage info image
                    when oPush $ pushRestylerImage image
                    pure $ toRestyler info image

                traverse_ (liftIO . (`Manifest.write` restylers)) oWrite
