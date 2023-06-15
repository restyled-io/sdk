module Restyled.Promote.Options
  ( Options (..)
  , parseOptions
  )
where

import RIO

import Options.Applicative
import Restyled.Promote.Channel
import Restyled.Promote.IntegrationTest

data Options = Options
  { oFromPath :: Maybe FilePath
  , oSkipIntegrationTest :: Bool
  , oIntegrationTestOptions :: IntegrationTestOptions
  , oProceed :: Bool
  , oFromChannel :: Channel
  , oToChannel :: Maybe Channel
  }

parseOptions :: IO Options
parseOptions = execParser $ parse options "Test and promote a Restylers set"

-- brittany-disable-next-binding

options :: Parser Options
options =
  Options
    <$> optional
      ( option
          str
          ( long "file"
              <> help "Upload PATH as FROM first"
              <> metavar "PATH"
          )
      )
    <*> switch
      ( long "no-test"
          <> help "Skip integration tests step"
      )
    <*> integrationOptions
    <*> switch
      ( long "yes"
          <> help "Confirm overwriting of TO"
      )
    <*> argument
      (eitherReader readChannel)
      ( help "Channel to promote from"
          <> metavar "FROM"
      )
    <*> optional
      ( argument
          (eitherReader readChannel)
          ( help "Channel to promote to"
              <> metavar "TO"
          )
      )

-- brittany-disable-next-binding

integrationOptions :: Parser IntegrationTestOptions
integrationOptions =
  integrationTest
    <$> option
      (eitherReader readIntegrationTest)
      ( long "test"
          <> help "Test to run"
          <> value IntegrationTestDemo45
          <> metavar "TESTNAME"
      )
    <*> option
      str
      ( long "image"
          <> help "Restyler image to test with"
          <> value "restyled/restyler:edge"
          <> metavar "IMAGE"
      )
    <*> switch
      ( long "debug"
          <> help "Run Restyler with DEBUG=1"
          <> showDefault
      )

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
