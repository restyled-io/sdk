module Restyled.Promote.Manifest
    ( withManifest
    , diffManifests
    , copyManifest
    , uploadManifest
    ) where

import RIO

import Conduit
import Network.HTTP.Simple
import qualified RIO.ByteString.Lazy as BSL
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T
import Restyled.Promote.Channel

withManifest :: MonadUnliftIO m => Channel -> (FilePath -> m a) -> m a
withManifest channel f =
    withSystemTempFile "promote-restylers-manifest" $ \tmp h -> do
        let req = parseRequest_ $ manifestUrl channel
        httpSink req $ \_resp -> sinkHandle h
        hFlush h
        f tmp

diffManifests
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => Channel
    -> Channel
    -> m ()
diffManifests fromChannel toChannel = withManifest fromChannel $ \updated ->
    withManifest toChannel $ \current -> void $ proc
        "diff"
        ["--unified", "--color=always", current, updated]
        runProcess

copyManifest
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Channel
    -> Channel
    -> m ()
copyManifest = copyManifest_ . Right

uploadManifest
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => FilePath
    -> Channel
    -> m ()
uploadManifest = copyManifest_ . Left

copyManifest_
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Either FilePath Channel
    -> Channel
    -> m ()
copyManifest_ fromItem toChannel = do
    bucket <- getStackOutput "sites-docs" "BucketName"
    distributionId <- getStackOutput "sites-docs" "DistributionId"

    let
        cpFrom = case fromItem of
            Left path -> path
            Right c -> unpack $ "s3://" <> bucket <> manifestPath c

    proc
        "aws"
        (concat
            [ ["s3", "cp"]
            , ["--acl", "public-read"]
            , ["--content-type", "text/plain"]
            , [cpFrom, unpack $ "s3://" <> bucket <> manifestPath toChannel]
            ]
        )
        runProcess_
    proc
        "aws"
        (concat
            [ ["cloudfront", "create-invalidation"]
            , ["--distribution-id", unpack distributionId]
            , ["--paths", unpack $ manifestPath toChannel]
            ]
        )
        runProcess_

manifestUrl :: Channel -> String
manifestUrl channel =
    "https://docs.restyled.io" <> unpack (manifestPath channel)

manifestPath :: Channel -> Text
manifestPath channel =
    "/data-files/restylers/manifests/"
        <> channelName channel
        <> "/restylers.yaml"

getStackOutput
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Text -- ^ Stack
    -> Text -- ^ Output name
    -> m Text
getStackOutput stack name =
    T.strip
        . decodeUtf8With lenientDecode
        . BSL.toStrict
        <$> proc
                "aws"
                (concat
                    [ ["cloudformation", "describe-stacks"]
                    , ["--stack-name", unpack stack]
                    , ["--query", outputQuery $ unpack name]
                    , ["--output", "text"]
                    ]
                )
                readProcessStdout_

outputQuery :: String -> String
outputQuery key = "Stacks[*].Outputs[?OutputKey==`" <> key <> "`].OutputValue"
