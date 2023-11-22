module Mach.Exec where

import Control.Exception (throwIO)
import Control.Monad (filterM, unless)
import Data.Maybe (catMaybes)
import Mach.Error (MakeErr (..), TargetError (NoTargetOrFile))
import Mach.Eval
import System.Directory (doesPathExist, getModificationTime)

-- Return all prerequisites which represent targets. If a prerequisite
-- is neither a target nor an existing file, then an error is thrown.
getTargetPreqs :: MkDef -> Target -> IO [Target]
getTargetPreqs mk target =
  catMaybes <$> mapM targetOrFile (getPreqs $ getDef target)
  where
    -- Lookup the given target. If neither a target nor a file
    -- with the given name exists, then throw an error. If no
    -- target but a file exists, return Nothing.
    targetOrFile :: String -> IO (Maybe Target)
    targetOrFile name = do
      t <- lookupRule mk name
      case t of
        Just x -> pure $ Just x
        Nothing -> do
          exists <- doesPathExist name
          if exists
            then pure Nothing
            else throwIO $ TargetErr (NoTargetOrFile name)

-- Return the names of all prerequisites that a newer than the given target.
newerPreqs :: Target -> IO [FilePath]
newerPreqs target = do
  targetTime <- getModificationTime (getName target)
  filterM (fmap (targetTime <) . getModificationTime) (getPreqs $ getDef target)

-- Check whether the given target is up-to-date, a target shall be
-- considered up-to-date if it exists and is newer than all of its
-- dependencies.
isUp2Date :: Target -> IO Bool
isUp2Date target = do
  exists <- doesPathExist (getName target)
  if not exists
    then pure False
    else null <$> newerPreqs target

-- Build a target if it isn't up-to-date.
maybeBuild :: MkDef -> Target -> IO ()
maybeBuild mk target = do
  -- Recursively ensure that all prerequisites are up-to-date.
  getTargetPreqs mk target >>= mapM_ (maybeBuild mk)

  up2Date <- isUp2Date target
  unless up2Date $
    runCmds (getCmds mk target)
