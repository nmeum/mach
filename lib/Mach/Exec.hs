module Mach.Exec where

import Control.Exception (throwIO)
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Mach.Error (MakeErr (..), TargetError (NoTargetOrFile))
import Mach.Eval
import System.Directory (doesPathExist, getModificationTime)

data FileTarget = FileTarget FilePath TgtDef

lookupTarget :: MkDef -> String -> Maybe FileTarget
lookupTarget mkDef name =
  FileTarget name <$> lookupRule mkDef name

-- Return all prerequisites which represent targets. If a prerequisite
-- is neither a target nor an existing file, then an error is thrown.
getTargetPreqs :: MkDef -> FileTarget -> IO [FileTarget]
getTargetPreqs mk (FileTarget _ target) =
  catMaybes <$> mapM targetOrFile (getPreqs target)
  where
    -- Lookup the given target. If neither a target nor a file
    -- with the given name exists, then throw an error. If no
    -- target but a file exists, return Nothing.
    targetOrFile :: String -> IO (Maybe FileTarget)
    targetOrFile name =
      case lookupTarget mk name of
        Just x -> pure $ Just x
        Nothing -> do
          exists <- doesPathExist name
          if exists
            then pure Nothing
            else throwIO $ TargetErr (NoTargetOrFile name)

-- Return the names of all prerequisites that a newer than the given target.
newerPreqs :: FileTarget -> IO [FilePath]
newerPreqs (FileTarget name target) = do
  targetTime <- getModificationTime name
  filterM (fmap (targetTime <) . getModificationTime) $ getPreqs target

-- Check whether the given target is up-to-date, a target shall be
-- considered up-to-date if it exists and is newer than all of its
-- dependencies.
isUp2Date :: FileTarget -> IO Bool
isUp2Date target@(FileTarget name _) = do
  exists <- doesPathExist name
  if not exists
    then pure False
    else null <$> newerPreqs target

-- Build a target if it isn't up-to-date. Returns true if the target
-- was build or false if it was already up-to-date and wasn't build.
maybeBuild :: MkDef -> FileTarget -> IO Bool
maybeBuild mk target@(FileTarget name tgtDef) = do
  -- Recursively ensure that all prerequisites are up-to-date.
  getTargetPreqs mk target >>= mapM_ (maybeBuild mk)

  up2Date <- isUp2Date target
  if up2Date
    then pure False
    else runCmds (getCmds mk name tgtDef) >> pure True
