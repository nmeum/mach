module Mach.Exec where

import Control.Monad (filterM)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import qualified Data.Text as T
import Mach.Eval
import Mach.Macro (expand)
import System.Directory (doesPathExist, getModificationTime)
import System.Process (callCommand)

data FileTarget = FileTarget FilePath TgtDef

lookupTarget :: MkDef -> T.Text -> Maybe FileTarget
lookupTarget (MkDef _ targets) name =
  FileTarget (unpack name) <$> Map.lookup name targets

newerPreqs :: FileTarget -> IO [FilePath]
newerPreqs (FileTarget name (Target preqs _)) = do
  targetTime <- getModificationTime name
  filterM (fmap (targetTime >) . getModificationTime) lst
  where
    lst = toList (unpack <$> preqs)

------------------------------------------------------------------------

buildTarget :: MkDef -> FileTarget -> IO ()
buildTarget (MkDef env _) (FileTarget _ (Target _ cmds)) = do
  -- TODO: Make sure each line is expanded individually
  -- Requires changes to the Makefile parser.
  let expanded = fmap (expand env) cmds
  mapM_ (callCommand . unpack) expanded

maybeBuild :: MkDef -> FileTarget -> IO Bool
maybeBuild mk target@(FileTarget name (Target preqs _)) = do
  targetExists <- doesPathExist name
  newerDepends <- newerPreqs target

  if targetExists && null newerDepends
    then pure False
    else do
      -- TODO: Complain about non-existing files for which no targets are defined.
      let depends = catMaybes $ toList $ fmap (lookupTarget mk) preqs
      mapM_ (maybeBuild mk) depends

      buildTarget mk target >> pure True
