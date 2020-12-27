{-# LANGUAGE NamedFieldPuns #-}

-- | Details about how to build a Restyler Docker image
module Restylers.Info.Build
    ( RestylerBuild(..)
    , restylerBuild
    , build
    )
where

import RIO

import Data.Aeson
import Restylers.Image
import RIO.FilePath (takeDirectory, (</>))
import RIO.Process
import RIO.Text (unpack)

data RestylerBuild = RestylerBuild
    { path :: FilePath
    , dockerfile :: FilePath
    , versionCache :: FilePath
    , options :: [String]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass FromJSON

restylerBuild :: FilePath -> RestylerBuild
restylerBuild yaml = RestylerBuild
    { path
    , dockerfile = path </> "Dockerfile"
    , versionCache = path </> ".version"
    , options = []
    }
    where path = takeDirectory yaml

build
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerBuild
    -> RestylerImage
    -> m RestylerImage
build RestylerBuild {..} image = image <$ proc "docker" args runProcess_
  where
    args = concat
        [ ["build", "--quiet"]
        , ["--tag", unImage image]
        , ["--file", dockerfile]
        , [path]
        ]

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
