module Restylers.Options
    ( Options(..)
    , Command(..)
    , HasOptions(..)
    , parseOptions
    ) where

import RIO

import Options.Applicative
import Restylers.Registry

data Options = Options
    { oRegistry :: Maybe Registry
    , oSha :: Text
    , oDebug :: Bool
    , oCommand :: Command
    , oInput :: FilePath
    }
    deriving stock Show

data Command
    = Lint
    | Test Bool Bool (Maybe FilePath)
    deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Process Restylers" options

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
    <*> subparser
        (  command "lint" (withInfo "" (pure Lint))
        <> command "test" (withInfo "" (Test
            <$> switch
                (  short 'b'
                <> long "build"
                <> help "Build before testing"
                )
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
                ))))
        )
    <*> argument str
        (  help "Path to Restyler info.yaml"
        <> metavar "PATH"
        )

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
