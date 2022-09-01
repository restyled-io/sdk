module Restylers.Version
    ( RestylerVersion(..)
    , toDataVersion
    ) where

import RIO

import Data.Aeson
import Data.Version
import qualified RIO.NonEmpty as NE
import RIO.Text (unpack)
import Text.ParserCombinators.ReadP (readP_to_S)

newtype RestylerVersion = RestylerVersion
    { unRestylerVersion :: Text
    }
    deriving newtype (Eq, Show, Display, FromJSON, ToJSON)

toDataVersion :: RestylerVersion -> Maybe Version
toDataVersion =
    fmap (fst . NE.last)
        . NE.nonEmpty
        . readP_to_S parseVersion
        . dropV
        . unpack
        . unRestylerVersion
  where
    dropV = \case
        ('v' : rest) -> rest
        rest -> rest
