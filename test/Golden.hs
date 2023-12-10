module Golden (eqivTests) where

import Control.Applicative ((<|>))
import Mach.Main (run)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath
import System.IO (hClose, hGetContents)
import System.Process
  ( StdStream (CreatePipe),
    createPipe,
    createProcess,
    cwd,
    proc,
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

runMach :: FilePath -> IO MakeResult
runMach skel = do
  destDir <- prepTempDir "actual" skel

  (readEnd, writeEnd) <- createPipe
  withCurrentDirectory destDir $ run writeEnd []

  -- The readEnd is semi-close by hGetContents, should be
  -- closed once the entire handle content has been read.
  out <- hGetContents readEnd <* hClose writeEnd

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
      runTest "double-suffix-inference",
      runTest "silent-exec"
    ]
