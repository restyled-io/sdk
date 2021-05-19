module Main
    ( main
    ) where

import RIO

import Restylers.App
import Restylers.Build
import qualified Restylers.Info.Resolved as Info
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
            info <- Info.load oInput

            when oBuild $ buildRestylerImage info

            image <- tagRestylerImage info
            testRestylerImage info image

            when oPush $ do
                exists <- doesRestylerImageExist image
                if exists
                    then logWarn "Not pushing, image exists"
                    else pushRestylerImage image

            let restylers = pure $ toRestyler info image
            traverse_ (liftIO . (`Manifest.write` restylers)) oWrite
