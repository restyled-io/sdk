module Restyled.Promote.Ruby
    ( ruby
    , rubyStdout
    )
where

import RIO

import qualified RIO.ByteString.Lazy as BSL
import RIO.Process
import qualified RIO.Text as T

ruby
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => [String]
    -> Text
    -> m ()
ruby requires rb =
    proc "ruby" (concatMap (\r -> ["-r", r]) requires) $ runProcess_ . setStdin
        (byteStringInput $ BSL.fromStrict $ encodeUtf8 rb)

rubyStdout
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => [String]
    -> Text
    -> m Text
rubyStdout requires rb = do
    bs <-
        proc "ruby" (concatMap (\r -> ["-r", r]) requires)
        $ readProcessStdout_
        . setStdin (byteStringInput $ BSL.fromStrict $ encodeUtf8 rb)
    pure $ T.strip $ decodeUtf8With lenientDecode $ BSL.toStrict bs
