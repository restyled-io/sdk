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
  , oTestCount :: TestCount
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
    <*> optTestCount
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

optTestCount :: Parser TestCount
optTestCount =
  go
    <$> switch
      ( long "no-test"
          <> help "Skip integration tests step"
      )
    <*> optional
      ( option
          auto
          (long "test-count" <> metavar "N" <> help "Run test for N random restylers")
      )
 where
  go b mtc = case (b, mtc) of
    (True, _) -> TestOnly 0
    (_, Nothing) -> TestAll
    (_, Just tc) -> TestOnly tc

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
