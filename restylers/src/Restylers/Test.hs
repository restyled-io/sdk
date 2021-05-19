module Restylers.Test
    ( testRestylerImage
    ) where

import           RIO

import           RIO.FilePath            (takeBaseName, (</>))
import           RIO.List                (nub)
import           RIO.Process
import           RIO.Text                (unpack)
import           Restylers.Directory
import           Restylers.Image
import qualified Restylers.Info.Metadata as Metadata
import           Restylers.Info.Resolved (RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import           Restylers.Info.Test     (ExpectationFailure, Test,
                                          assertTestRestyled, testFilePath,
                                          writeTestFiles)

testRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => RestylerInfo
    -> RestylerImage
    -> m ()
testRestylerImage info image = do
    let tests = zip [1 ..] $ Metadata.tests $ Info.metadata info

    logInfo
        $ "Running "
        <> displayShow (length tests)
        <> " test(s) for "
        <> display info
        <> " with "
        <> display image

    chd <- getCurrentHostDirectory
    cwd <- getCurrentDirectory

    withTempDirectory cwd "restylers-test" $ \tmp ->
        withCurrentDirectory tmp $ do
            runRestyler (chd </> takeBaseName tmp) info image tests

            for_ tests $ \(number, test) -> do
                eResult <- try $ assertTestRestyled number (Info.name info) test
                either
                    (\ex -> do
                        logError "Failed"
                        logError $ display @ExpectationFailure ex
                        exitFailure
                    )
                    (\_ -> logInfo "Passed")
                    eResult

runRestyler
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => FilePath
    -> RestylerInfo
    -> RestylerImage
    -> [(Int, Test)]
    -> m ()
runRestyler code info image tests = do
    for_ tests $ \(number, test) -> do
        writeTestFiles number (Info.name info) test


    if Info.supports_multiple_paths info
        then dockerRun relativePaths
        else traverse_ (dockerRun . pure) relativePaths
  where
    -- Restyler prepends ./, so we do too
    relativePaths = map
        (\(number, test) -> "./" <> testFilePath number (Info.name info) test)
        tests

    -- Restyler uniques the created arguments, so we do too
    dockerRun paths = proc
        "docker"
        (nub $ concat
            [ ["run", "--interactive", "--rm"]
            , ["--net", "none"]
            , ["--volume", code <> ":/code"]
            , [unpack $ unRestylerImage image]
            , map unpack $ Info.command info
            , map unpack $ Info.arguments info
            , [ "--" | Info.supports_arg_sep info ]
            , paths
            ]
        )
        runProcess_
