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
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (callCommand)

data FileTarget = FileTarget FilePath TgtDef

lookupTarget :: MkDef -> T.Text -> Maybe FileTarget
lookupTarget (MkDef _ targets) name =
  FileTarget (unpack name) <$> Map.lookup name targets

-- Lookup the given target. If neither a target nor a file
-- with the given name exists, then throw an error. If no
-- target but a file exists, return Nothing.
targetOrFile :: MkDef -> T.Text -> IO (Maybe FileTarget)
targetOrFile mk name =
  case lookupTarget mk name of
    Just x -> pure $ Just x
    Nothing -> do
      exists <- doesPathExist $ unpack name
      -- TODO: Throw a custom exception here
      if exists
        then pure Nothing
        else fail $ "no target or file named " ++ show name

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

  -- Make sure newerDepends is lazy evaluated. Otherwise,
  -- the file may not exist and getModificationTime fails.
  newerDepends <- unsafeInterleaveIO $ newerPreqs target

  if targetExists && null newerDepends
    then pure False
    else do
      depends <- mapM (targetOrFile mk) preqs
      mapM_ (maybeBuild mk) (catMaybes $ toList depends)

      buildTarget mk target >> pure True
