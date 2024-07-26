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

    unless (oTestCount == TestOnly 0)
      $ runIntegrationTest oFromChannel oTestCount oRestyleCmd

    for_ oToChannel $ \channel -> do
      diffManifests oFromChannel channel
      when oProceed $ copyManifest oFromChannel channel
