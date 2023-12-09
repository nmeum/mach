module Golden (eqivTests) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import GHC.IO.Handle (hGetContents)
import Mach.Main (run)
import System.Directory
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath
import System.IO.Silently (capture_)
import System.Process
import Test.Tasty
import Test.Tasty.Golden.Advanced

getTestDir :: IO FilePath
getTestDir = do
  tempDir <- getTemporaryDirectory
  pure $ tempDir </> "mach-tests"

prepTempDir :: String -> FilePath -> IO FilePath
prepTempDir name skel = do
  tempDir <- (</> name) <$> getTestDir
  removePathForcibly tempDir
    >> createDirectoryIfMissing True tempDir

  -- Copy files from the skeleton dir to the tempDir.
  -- Only works for regular files, probably doesn't recurse.
  void $ listDirectory skel >>= mapM (\n -> copyFile (skel </> n) $ tempDir </> takeFileName n)

  pure tempDir

------------------------------------------------------------------------

type MakeResult = (String, FilePath)

runMach :: FilePath -> IO MakeResult
runMach skel = do
  destDir <- prepTempDir "actual" skel

  out <- capture_ (withCurrentDirectory destDir $ run [])
  pure (out, destDir)

runGolden :: FilePath -> IO MakeResult
runGolden skel = do
  destDir <- prepTempDir "golden" skel

  (_, Just hout, _, p) <-
    createProcess
      (proc "make" [])
        { cwd = Just destDir,
          std_out = CreatePipe
        }

  exitCode <- waitForProcess p
  case exitCode of
    ExitSuccess -> do
      out <- hGetContents hout
      pure (out, destDir)
    ExitFailure _ -> ioError (userError "runGolden: non-zero exit")

compareRuns :: MakeResult -> MakeResult -> IO (Maybe String)
compareRuns (outG, fpG) (outA, fpA) = do
  compareDirectory
  where
    -- TODO: Comparing stdout doesn't work correctly at the moment.
    --       Probably an issue with System.IO.Silently.
    --
    -- out <- compareStdout
    -- dir <- compareDirectory
    -- pure (out <|> dir)

    compareStdout :: IO (Maybe String)
    compareStdout =
      pure $
        if outG /= outA
          then Just "standard output differs"
          else Nothing

    compareDirectory :: IO (Maybe String)
    compareDirectory = do
      let diffArgs = ["-upr", fpG, fpA]
      (_, Just hout, _, p) <-
        createProcess
          (proc "diff" diffArgs) {std_out = CreatePipe}

      exitCode <- waitForProcess p
      case exitCode of
        ExitSuccess -> pure Nothing
        ExitFailure _ -> Just <$> hGetContents hout

runMake :: TestName -> FilePath -> TestTree
runMake name makeDir =
  goldenTest
    name
    (runGolden makeDir)
    (runMach makeDir)
    compareRuns
    (\_ -> pure ())

------------------------------------------------------------------------

runTest :: String -> TestTree
runTest name = runMake name $ "test" </> "golden" </> name

eqivTests :: TestTree
eqivTests =
  testGroup
    "eviqTests"
    [ runTest "expand-delayed",
      runTest "expand-immediate",
      runTest "expand-conditional",
      runTest "default-rule",
      runTest "include-makefile",
      runTest "single-suffix-inference",
      runTest "double-suffix-inference"
    ]
