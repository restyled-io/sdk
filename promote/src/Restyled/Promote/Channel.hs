module Restyled.Promote.Channel
  ( Channel (..)
  , channelName
  , readChannel
  )
where

import RIO

import RIO.Text (pack)

data Channel
  = Dev
  | Stable
  | Custom Text

instance Display Channel where
  display = display . channelName

channelName :: Channel -> Text
channelName = \case
  Dev -> "dev"
  Stable -> "stable"
  Custom t -> t

readChannel :: String -> Either String Channel
readChannel = \case
  "dev" -> Right Dev
  "stable" -> Right Stable
  x -> Right $ Custom $ pack x
