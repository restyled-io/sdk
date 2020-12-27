module Restylers.Options
    ( Command(..)
    , Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative
import Restylers.Registry
import RIO.NonEmpty (some1)

data Command
    = Build FilePath
    | Release FilePath (NonEmpty FilePath)
    deriving stock Show

data Options = Options
    { oRegistry :: Maybe Registry
    , oTag :: Text
    , oDebug :: Bool
    , oCommand :: Command
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
    <*> subparser
        (  command "build" (parse
            (Build <$> yamlArgument)
            "Build an image for Restylers described in info.yaml files")
        <> command "release" (parse
            (Release
                <$> strOption
                    (  short 'w'
                    <> long "write"
                    <> help "File released Restylers to file"
                    <> metavar "PATH"
                    <> value "restylers.yaml"
                    )
                <*> some1 yamlArgument)
            "Released (push) versioned images")
        )

yamlArgument :: Parser FilePath
yamlArgument =
    argument str (help "Path to Restyler info.yaml" <> metavar "PATH")

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
