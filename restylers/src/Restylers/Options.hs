module Restylers.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative
import Restylers.Registry
import RIO.NonEmpty (some1)

data Options = Options
    { oRegistry :: Maybe Registry
    , oTag :: Text
    , oDebug :: Bool
    , oPush :: Bool
    , oAlways :: Bool
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
        (  short 't'
        <> long "tag"
        <> help "Tag to use for development images"
        <> metavar "TAG"
        <> value "dev"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Log more verbosity"
        )
    <*> switch
        (  short 'p'
        <> long "push"
        <> help "Push built images"
        )
    <*> switch
        (  short 'a'
        <> long "always"
        <> help "Build and push even if image already exists"
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
