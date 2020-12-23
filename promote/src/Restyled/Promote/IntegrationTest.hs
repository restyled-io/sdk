{-# LANGUAGE QuasiQuotes #-}

module Restyled.Promote.IntegrationTest
    ( IntegrationTests(..)
    , integrationTest
    , readIntegrationTest
    , IntegrationTestOptions(..)
    , runIntegrationTest
    )
where

import RIO

import Restyled.Promote.Channel
import Restyled.Promote.Manifest
import Restyled.Promote.Ruby
import qualified RIO.ByteString.Lazy as BSL
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T
import Text.Shakespeare.Text (st)

data IntegrationTests
    = IntegrationTestDemo45

readIntegrationTest :: String -> Either String IntegrationTests
readIntegrationTest = \case
    "demo45" -> Right IntegrationTestDemo45
    x -> Left $ "Invalid test " <> show x

integrationTest :: IntegrationTests -> Text -> Bool -> IntegrationTestOptions
integrationTest IntegrationTestDemo45 image debug = IntegrationTestOptions
    { oitApp = "dev"
    , oitInstallationId = 58920
    , oitRepo = "restyled-io/demo"
    , oitBranch = "testing/all"
    , oitPullRequest = 45
    , oitRestylerImage = image
    , oitDebug = debug
    }

data IntegrationTestOptions = IntegrationTestOptions
    { oitApp :: Text
    , oitInstallationId :: Int
    , oitRepo :: Text
    , oitBranch :: Text
    , oitPullRequest :: Int
    , oitRestylerImage :: Text
    , oitDebug :: Bool
    }

runIntegrationTest
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => Channel
    -> IntegrationTestOptions
    -> m ()
runIntegrationTest channel IntegrationTestOptions {..} = do
    let pr = oitRepo <> "#" <> tshow oitPullRequest

    logInfo $ "Channel: " <> display channel
    logInfo $ "Restyler Image: " <> display oitRestylerImage
    logInfo $ "Pull Request: " <> display pr

    token <- getAccessToken oitApp oitInstallationId
    withManifest channel $ \tmp ->
        ruby ["fileutils", "tmpdir", "yaml"]
            $ setupIntegrationRb tmp channel token oitRepo oitBranch

    proc
        "docker"
        (concat
            [ ["run", "--interactive", "--tty", "--rm"]
            , if oitDebug then ["--env", "DEBUG=1"] else []
            , ["--env", "GITHUB_ACCESS_TOKEN=" <> unpack token]
            , ["--volume", "/tmp:/tmp"]
            , ["--volume", "/var/run/docker.sock:/var/run/docker.sock"]
            , [unpack oitRestylerImage]
            , ["--job-url", "https://example.com"]
            , ["--color=always", unpack pr]
            ]
        )
        runProcess_

getAccessToken
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Text
    -> Int
    -> m Text
getAccessToken env installationId =
    T.strip
        . decodeUtf8With lenientDecode
        . BSL.toStrict
        <$> proc
                "restyled"
                ["get-access-token", unpack env, show installationId]
                readProcessStdout_

setupIntegrationRb
    :: FilePath
    -> Channel
    -> Text -- ^ Token
    -> Text -- ^ owner/name
    -> Text -- ^ branch
    -> Text
setupIntegrationRb manifest channel token repo branch = [st|
FILES = {}

def system_(*args)
  system(*args) or raise "system error: " + args.inspect
end

restylers = YAML.load_file("#{manifest}").map do |restyler|
  name = restyler.fetch("name")
  tests = restyler.
    fetch("metadata", {}).
    fetch("tests", [])

  tests.each.with_index do |test, n|
    # N.B. if different cases need the same support file this won't work.
    # If/when that happens we can consider some kind of workaround.
    if support = test["support"]
      support_path = support.fetch("path")
      FILES[support_path] = support.fetch("contents")
    end

    ext = test["extension"] || "example"
    path = name + "/test-file-" + n.to_s + "." + ext
    contents = test.fetch("contents")

    # Can't commit CRLF, so skip that test case
    unless contents.include?("\r\n")
      FILES[path] = contents
    end
  end

  {
    name => {
      "enabled" => true,
      "include" => [name + "/**/*"]
    }
  }
end

FILES[".restyled.yaml"] = YAML.dump({
  "restylers_version" => "#{channelName channel}",
  "restylers" => restylers
})

Dir.mktmpdir do |dir|
  url = "https://x-access-token:#{token}@github.com/#{repo}.git"
  system_("git", "clone", url, dir)

  Dir.chdir(dir) do
    system("git", "checkout", "--quiet", "#{branch}") or
      system_("git", "checkout", "--quiet", "-b", "#{branch}")

    # Clean directories, to be recreated
    FILES.keys.map { |p| File.dirname(p) }.uniq.each do |d|
      if d != '.'
        FileUtils.rm_rf d
      end
    end

    FILES.each do |path, contents|
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, contents)
    end

    system_("git", "add", ".")

    if system("git", "commit", "-m", "Update test case files")
      system_("git", "push")
    end
  end
end
|]
