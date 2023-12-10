module Util where

import Control.Monad (void)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    getTemporaryDirectory,
    listDirectory,
    removePathForcibly,
  )
import System.FilePath (takeFileName, (</>))

-- Path to temporary directory for golden tests.
getTestDir :: IO FilePath
getTestDir = do
  tempDir <- getTemporaryDirectory
  pure $ tempDir </> "mach-tests"

-- Copy files in skelekton directory to a subdirectory with the
-- given name in a temporary directory obtained via 'getTestDir'.
prepTempDir :: String -> FilePath -> IO FilePath
prepTempDir name skel = do
  tempDir <- (</> name) <$> getTestDir
  removePathForcibly tempDir
    >> createDirectoryIfMissing True tempDir

  -- Copy files from the skeleton dir to the tempDir.
  -- Only works for regular files, probably doesn't recurse.
  void $ listDirectory skel >>= mapM (\n -> copyFile (skel </> n) $ tempDir </> takeFileName n)

  pure tempDir
