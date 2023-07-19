module Main
  ( main
  ) where

import RIO

import RIO.Directory (doesFileExist)
import RIO.FilePath (takeExtension, (<.>), (</>))
import Restylers.App
import Restylers.Build
import Restylers.Image (getSeriesImages)
import qualified Restylers.Info.Resolved as Info
import Restylers.Manifest (toRestyler)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Test
import Restylers.UpdateCheck

main :: IO ()
main = do
  opts@Options {..} <- parseOptions
  logOptions <- logOptionsHandle stdout oDebug
  withLogFunc logOptions $ \lf -> do
    app <- loadApp opts lf
    runRIO app $ do
      logDebug $ "Options: " <> displayShow opts
      restylers <- for oInput $ \path -> do
        yaml <- locateYaml path
        info <- Info.load yaml
        when oBuild $ buildRestylerImage info
        image <- tagRestylerImage info
        when oCheckForUpdate $ checkForUpdate info image
        pure $ toRestyler info image

      testRestylers restylers $ fromMaybe [] oHspecArgs

      when oPush $ for_ restylers $ \restyler -> do
        exists <- doesRestylerImageExist $ Manifest.image restyler
        if exists
          then logWarn "Not pushing, image exists"
          else do
            pushRestylerImage $ Manifest.image restyler

            for_ (getSeriesImages $ Manifest.image restyler) $ \images -> do
              for_ images $ \image -> do
                logInfo $ "Updating series image " <> display image
                dockerTag (Manifest.image restyler) image
                pushRestylerImage image

      traverse_ (liftIO . (`Manifest.write` restylers)) oWrite

locateYaml
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m FilePath
locateYaml input
  | takeExtension input `elem` [".yml", ".yaml"] = pure input
  | otherwise = do
      let input' = input </> "info" <.> "yaml"
      exists <- doesFileExist input'
      input' <$ unless exists err
 where
  err :: (MonadIO m, MonadReader env m, HasLogFunc env) => m ()
  err = do
    logError
      $ "Invalid PATH input ("
      <> fromString input
      <> "). Must be .yml, .yaml, or a directory containing an info.yaml"
    exitFailure
