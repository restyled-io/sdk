module Restylers.UpdateCheck
  ( checkForUpdate
  ) where

import RIO

import Control.Lens (_head)
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Version
import Network.HTTP.Simple
  ( addRequestHeader
  , getResponseBody
  , httpJSON
  , parseRequest_
  )
import Network.HTTP.Types.Header (hUserAgent)
import RIO.List (headMaybe, sortOn)
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T
import Restylers.Build
import Restylers.Image
import Restylers.Info.Metadata (Upstream (..))
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Info.Resolved (ImageSource (..), RestylerInfo (..))
import qualified Restylers.Info.Resolved as Info
import Restylers.Name (RestylerName (..))
import Restylers.Version (RestylerVersion (..), toDataVersion)

checkForUpdate
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerInfo
  -> RestylerImage
  -> m ()
checkForUpdate info image = do
  mVersions <-
    liftA2 (,) <$> getCurrentVersion info image <*> getRemoteVersion info

  for_ mVersions $ \(current, remote) -> do
    when (remote > current)
      $ throwString
      $ "Newer version available for "
      <> unpack (unRestylerName $ Info.name info)
      <> ":"
      <> showVersion current
      <> " < "
      <> showVersion remote

getCurrentVersion
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerInfo
  -> RestylerImage
  -> m (Maybe Version)
getCurrentVersion info image = case Info.imageSource info of
  Explicit {} -> pure Nothing
  BuildVersionCmd _ cmd _ ->
    toDataVersion . RestylerVersion <$> dockerRunSh image cmd
  BuildVersion _ explicitVersion _ -> pure $ toDataVersion explicitVersion

getRemoteVersion :: MonadIO m => RestylerInfo -> m (Maybe Version)
getRemoteVersion info = do
  mResult <- for (Metadata.upstream $ Info.metadata info) $ \case
    Hackage -> do
      val <-
        httpJSON @_ @Value
          $ parseRequest_
          $ unpack
          $ "https://hackage.haskell.org/package/"
          <> unRestylerName (Info.name info)
          <> "/preferred"

      pure $ do
        rv <-
          getResponseBody val
            ^? key "normal-version"
              . _Array
              . _head
              . _JSON

        toDataVersion rv
    Git orgRepo -> do
      val <-
        httpJSON @_ @Value
          $ addRequestHeader hUserAgent "Restyled/SDK"
          $ parseRequest_
          $ unpack
          $ "https://api.github.com/repos/"
          <> orgRepo
          <> "/git/refs/tags"

      let rvs =
            getResponseBody val
              ^.. _Array
                . traverse
                . key "ref"
                . _String
                . to (RestylerVersion . T.dropPrefix "refs/tags/")

      pure
        $ headMaybe
        $ sortOn Down
        $ filter (not . isPrerelease)
        $ mapMaybe toDataVersion rvs

  pure $ join mResult

isPrerelease :: Version -> Bool
isPrerelease Version {..} = any (`elem` versionTags) prereleaseTags

prereleaseTags :: [String]
prereleaseTags = ["alpha", "beta", "prerelease", "rc"]
