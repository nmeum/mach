module Golden (eqivTests) where

import Control.Applicative ((<|>))
import Mach.Main (run)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath
import System.IO (IOMode (WriteMode), hClose, hGetContents, openFile)
import System.Process
  ( StdStream (CreatePipe, UseHandle),
    createPipe,
    createProcess,
    cwd,
    proc,
    std_err,
    std_out,
    waitForProcess,
  )
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Util

-- The golden test do not use a golden expected file but instead compare
-- the "output" of a reference make(1) implementation and mach. The
-- output is defined by 'MakeResult' and presently a tuple consisting of
-- standard output as a 'String' and a 'FilePath' which refers to a
-- temporary directory where make(1) was invoked. The directory is
-- compared using `diff -r`.
type MakeResult = (String, FilePath)

runMach :: [String] -> FilePath -> IO MakeResult
runMach flags skel = do
  destDir <- prepTempDir "actual" skel

  (readEnd, writeEnd) <- createPipe
  withCurrentDirectory destDir $ run writeEnd flags

  -- The readEnd is semi-close by hGetContents, should be
  -- closed once the entire handle content has been read.
  out <- hGetContents readEnd <* hClose writeEnd

  pure (out, destDir)

runGolden :: [String] -> FilePath -> IO MakeResult
runGolden flags skel = do
  destDir <- prepTempDir "golden" skel
  devNull <- openFile "/dev/null" WriteMode

  (_, Just hout, _, p) <-
    createProcess
      (proc "pdpmake" flags)
        { cwd = Just destDir,
          std_out = CreatePipe,
          std_err = UseHandle devNull
        }

  exitCode <- waitForProcess p <* hClose devNull
  case exitCode of
    ExitSuccess -> do
      out <- hGetContents hout
      pure (out, destDir)
    ExitFailure _ -> ioError (userError "runGolden: non-zero exit")

compareRuns :: MakeResult -> MakeResult -> IO (Maybe String)
compareRuns (outG, fpG) (outA, fpA) = do
  out <- compareStdout
  dir <- compareDirectory
  pure (out <|> dir)
  where
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

runMake :: TestName -> [String] -> FilePath -> TestTree
runMake name flags makeDir =
  goldenTest
    name
    (runGolden flags makeDir)
    (runMach flags makeDir)
    compareRuns
    (\_ -> pure ())

------------------------------------------------------------------------

runTest :: String -> [String] -> TestTree
runTest name flags = runMake name flags $ "test" </> "golden" </> name

eqivTests :: TestTree
eqivTests =
  testGroup
    "eviqTests"
    [ runTest "expand-delayed" [],
      runTest "expand-immediate" [],
      runTest "expand-conditional" [],
      runTest "default-rule" [],
      runTest "include-makefile" [],
      runTest "single-suffix-inference" [],
      runTest "double-suffix-inference" [],
      runTest "silent-exec" [],
      runTest "ignore-error" [],
      runTest "ignore-error-silent" [],
      runTest "silent-selected-targets" [],
      runTest "silent-all" [],
      runTest "silent-all-cmdline" ["-s"],
      runTest "expand-append" [],
      runTest "substitute-expand" [],
      runTest "builtin-c-compilation1" [],
      runTest "builtin-c-compilation2" [],
      runTest "silent-append" [],
      runTest "ignore-all" [],
      runTest "ignore-single" [],
      runTest "ignore-all-cmdline" ["-i"]
      -- runTest "append-prerequisites",
    ]
