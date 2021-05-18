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
    , oLint :: Bool
    , oBuild :: Bool
    , oTest :: Bool
    , oPush :: Bool
    , oWrite :: Maybe FilePath
    , oInputs :: NonEmpty FilePath
    }
    deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ parse options "Process Restylers"

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
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Log more verbosity"
        )
    <*> switch
        (  short 'l'
        <> long "lint"
        <> help "Lint Dockerfiles"
        )
    <*> switch
        (  short 'b'
        <> long "build"
        <> help "Build images"
        )
    <*> switch
        (  short 'T'
        <> long "test"
        <> help "Test built images"
        )
    <*> switch
        (  short 'p'
        <> long "push"
        <> help "Push built images"
        )
    <*> optional (strOption
        (  short 'w'
        <> long "write"
        <> help "Output restylers.yaml to PATH"
        <> metavar "PATH"
        ))
    <*> some1 (argument str
        (  help "Path to Restyler info.yaml"
        <> metavar "PATH"
        ))

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
