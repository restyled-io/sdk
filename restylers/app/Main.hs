module Main
    ( main
    ) where

import RIO

import RIO.Process
import Restylers.App
import Restylers.Build
import Restylers.Info.Resolved (RestylerInfo)
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
            info <- Info.load oInput

            case oCommand of
                Lint -> lintRestyler info
                Test b p w -> testRestyler b p w info

testRestyler
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => Bool
    -> Bool
    -> Maybe FilePath
    -> RestylerInfo
    -> m ()
testRestyler build push mWrite info = do
    when build $ buildRestylerImage info

    image <- tagRestylerImage info
    testRestylerImage info image

    when push $ do
        exists <- doesRestylerImageExist image
        if exists
            then logWarn "Not pushing, image exists"
            else pushRestylerImage image

    let restylers = pure $ toRestyler info image
    traverse_ (liftIO . (`Manifest.write` restylers)) mWrite
