module Main
  ( main
  )
where

import RIO

import Restyled.Promote.IntegrationTest
import Restyled.Promote.Manifest
import Restyled.Promote.Options

main :: IO ()
main = do
  Options {..} <- parseOptions

  runSimpleApp $ do
    traverse_ (`uploadManifest` oFromChannel) oFromPath

    unless oSkipIntegrationTest
      $ runIntegrationTest oFromChannel oIntegrationTestOptions

    for_ oToChannel $ \channel -> do
      diffManifests oFromChannel channel
      when oProceed $ copyManifest oFromChannel channel
