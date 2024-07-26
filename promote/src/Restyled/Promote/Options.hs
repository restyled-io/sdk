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
  , oRestyleCmd :: RestyleCmd
  , oProceed :: Bool
  , oFromChannel :: Channel
  , oToChannel :: Maybe Channel
  }

parseOptions :: IO Options
parseOptions = execParser $ parse options "Test and promote a Restylers set"

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
    <*> optRestyleCmd
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

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
