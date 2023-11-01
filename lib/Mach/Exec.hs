module Mach.Exec where

import Control.Exception (throwIO)
import Control.Monad (filterM)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Mach.Error (MakeErr (..), TargetError (NoTargetOrFile))
import Mach.Eval
import System.Directory (doesPathExist, getModificationTime)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (callCommand)

data FileTarget = FileTarget FilePath TgtDef

lookupTarget :: MkDef -> String -> Maybe FileTarget
lookupTarget (MkDef _ _ targets) name =
  FileTarget name <$> Map.lookup name targets

-- Lookup the given target. If neither a target nor a file
-- with the given name exists, then throw an error. If no
-- target but a file exists, return Nothing.
targetOrFile :: MkDef -> String -> IO (Maybe FileTarget)
targetOrFile mk name =
  case lookupTarget mk name of
    Just x -> pure $ Just x
    Nothing -> do
      exists <- doesPathExist name
      -- TODO: Throw a custom exception here
      if exists
        then pure Nothing
        else throwIO $ TargetErr (NoTargetOrFile name)

newerPreqs :: FileTarget -> IO [FilePath]
newerPreqs (FileTarget name (Target preqs _)) = do
  targetTime <- getModificationTime name
  filterM (fmap (targetTime >) . getModificationTime) preqs

------------------------------------------------------------------------

-- | Build a given target, including all dependencies that need to be rebuild.
buildTarget :: MkDef -> FileTarget -> IO ()
buildTarget mk (FileTarget _ target@(Target preqs _)) = do
  depends <- mapM (targetOrFile mk) preqs
  mapM_ (maybeBuild mk) (catMaybes depends)

  -- Actually build the target itself
  mapM_ callCommand $ getCmds mk target

maybeBuild :: MkDef -> FileTarget -> IO Bool
maybeBuild mk target@(FileTarget name (Target _ _)) = do
  targetExists <- doesPathExist name

  -- Make sure newerDepends is lazy evaluated. Otherwise,
  -- the file may not exist and getModificationTime fails.
  newerDepends <- unsafeInterleaveIO $ newerPreqs target

  if targetExists && null newerDepends
    then pure False
    else buildTarget mk target >> pure True
