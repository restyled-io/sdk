module Restylers.Info.Metadata
  ( Metadata (..)
  , Upstream (..)
  , emptyMetadata
  ) where

import RIO

import Data.Aeson
import Restylers.Info.Test

data Metadata = Metadata
  { upstream :: Maybe Upstream
  , languages :: [Text]
  , tests :: [Test]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Upstream
  = Hackage
  | -- | Org/Repo
    Git Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

emptyMetadata :: Metadata
emptyMetadata = Metadata {upstream = Nothing, languages = [], tests = []}
