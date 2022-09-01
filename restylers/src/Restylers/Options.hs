module Restylers.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    ) where

import RIO

import Options.Applicative
import RIO.NonEmpty (some1)
import Restylers.Registry

data Options = Options
    { oRegistry :: Maybe Registry
    , oSha :: Text
    , oDebug :: Bool
    , oBuild :: Bool
    , oPush :: Bool
    , oWrite :: Maybe FilePath
    , oCheckForUpdate :: Bool
    , oInput :: NonEmpty FilePath
    }
    deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Build, test, and push Restylers" options

-- brittany-disable-next-binding

options :: Parser Options
options = Options
    <$> optional (Registry <$> strOption
        (  short 'r'
        <> long "registry"
        <> help "Registry to prefix all Docker images"
        <> metavar "PREFIX"
        ))
    <*> strOption
        (  short 's'
        <> long "sha"
        <> help "Commit SHA to use as tag for input images"
        <> metavar "SHA"
        <> value "dev"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Log more verbosity"
        )
    <*> (not <$> switch
        (  short 'B'
        <> long "no-build"
        <> help "Skip build before testing"
        ))
    <*> switch
        (  short 'p'
        <> long "push"
        <> help "Push version-tagged image"
        )
    <*> optional (strOption
        (  short 'w'
        <> long "write"
        <> help "Output restyler definition to PATH"
        <> metavar "PATH"
        ))
    <*> switch
        (  long "check-for-update"
        <> help "Check for updates if possible, fail if available"
        )
    <*> some1 (argument str
        (  help "Path to Restyler info.yaml"
        <> metavar "PATH"
        ))

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
