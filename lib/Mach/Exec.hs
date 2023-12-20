module Mach.Exec
  ( mkConfig,
    maybeBuild,
    targetOrFile,
    ExecConfig,
  )
where

import Control.Exception (throwIO)
import Control.Monad (filterM, unless, when)
import Data.Maybe (catMaybes)
import Mach.Error (MakeErr (..))
import Mach.Eval
import qualified Mach.Types as T
import System.Directory (doesPathExist, getModificationTime)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hFlush, hPutStrLn, stdout)
import System.Process (ProcessHandle, StdStream (UseHandle), createProcess_, shell, std_out, waitForProcess)

-- | Configuration regarding the execution of Makefiles.
data ExecConfig = ExecConfig
  { -- | Handle used for all output
    output :: Handle,
    -- | Command line flags.
    flags :: [T.Flag],
    -- | Silent targets (.SILENT special target)
    silenced :: Maybe [String],
    -- | Ignored targets (.IGNORE special target)
    ignored :: Maybe [String],
    -- | .PHONY targets.
    phonies :: [String]
  }

mkConfig :: MkDef -> Handle -> [T.Flag] -> ExecConfig
mkConfig mkDef handle cflags =
  let ignAll = not $ null [() | T.IgnoreAll <- cflags]
      slnAll = not $ null [() | T.SilentAll <- cflags]
   in ExecConfig
        { output = handle,
          flags = cflags,
          silenced = if slnAll then Just [] else silent mkDef,
          ignored = if ignAll then Just [] else ignore mkDef,
          phonies = phony mkDef
        }

isSilent :: ExecConfig -> Target -> Bool
isSilent ExecConfig {silenced = s} tgt =
  case s of
    Nothing -> False
    Just [] -> True
    Just xs -> getName tgt `elem` xs

isIgnored :: ExecConfig -> Target -> Bool
isIgnored ExecConfig {ignored = s} tgt =
  case s of
    Nothing -> False
    Just [] -> True
    Just xs -> getName tgt `elem` xs

isPhony :: ExecConfig -> Target -> Bool
isPhony ExecConfig {phonies = lst} tgt = getName tgt `elem` lst

makeProc :: ExecConfig -> String -> IO ProcessHandle
makeProc ExecConfig {output = handle} cmd = do
  (_, _, _, p) <-
    if handle == stdout
      then createProcess_ [] (shell cmd)
      else createProcess_ [] (shell cmd) {std_out = UseHandle handle}
  pure p

runCmd :: ExecConfig -> Target -> Cmd -> IO ()
runCmd conf@ExecConfig {output = handle} tgt cmd = do
  unless (cmdSilent cmd || isSilent conf tgt) $
    (hPutStrLn handle (show cmd) >> hFlush handle)

  p <- makeProc conf $ cmdShell cmd
  exitCode <- waitForProcess p
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      unless (cmdIgnore cmd || isIgnored conf tgt) $
        throwIO $
          ExecErr ("non-zero exit: " ++ show cmd)

runTarget :: ExecConfig -> MkDef -> Target -> IO ()
runTarget conf mk tgt = mapM_ (runCmd conf tgt) (getCmds mk tgt)

------------------------------------------------------------------------

-- Lookup the given target. If neither a target nor a file with
-- the given name exists, then return the default target or throw
-- an error. If no target but a file exists, return Nothing.
targetOrFile :: MkDef -> String -> IO (Maybe Target)
targetOrFile mk name = do
  t <- lookupRule mk name
  case t of
    Just x -> pure $ Just x
    Nothing -> do
      exists <- doesPathExist name
      if exists
        then pure Nothing
        else either (throwIO . TargetErr) (pure . Just) $ defaultTarget name mk

-- Return all prerequisites which represent targets. If a prerequisite
-- is neither a target nor an existing file, then an error is thrown.
getTargetPreqs :: MkDef -> Target -> IO [Target]
getTargetPreqs mk target =
  catMaybes <$> mapM (targetOrFile mk) (getPreqs $ getDef target)

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
maybeBuild :: ExecConfig -> MkDef -> Target -> IO ()
maybeBuild conf mk target = do
  -- Recursively ensure that all prerequisites are up-to-date.
  getTargetPreqs mk target >>= mapM_ (maybeBuild conf mk)

  up2Date <- isUp2Date target
  when (not up2Date || isPhony conf target) $
    runTarget conf mk target
