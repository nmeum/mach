module Mach.Exec where

import Control.Monad (filterM)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Text (unpack)
import qualified Data.Text as T
import Mach.Eval
import System.Directory (doesPathExist, getModificationTime)

data FileTarget = FileTarget FilePath TgtDef

lookup :: MkDef -> T.Text -> Maybe FileTarget
lookup (MkDef _ targets) name =
  FileTarget (unpack name) <$> Map.lookup name targets

newerPreqs :: FileTarget -> IO [FilePath]
newerPreqs (FileTarget name (Target preqs _)) = do
  targetTime <- getModificationTime name
  filterM (fmap (targetTime >) . getModificationTime) lst
  where
    lst = toList (unpack <$> preqs)

------------------------------------------------------------------------

buildTarget :: MkDef -> FileTarget -> IO ()
buildTarget = error "not implemented"

maybeBuild :: MkDef -> FileTarget -> IO (Bool)
maybeBuild mk target@(FileTarget name _) = do
  targetExists <- doesPathExist name
  -- TODO: Actually need to build the prerequisites here.
  preqsTargets <- newerPreqs target

  if targetExists && null preqsTargets
    then pure False
    else buildTarget mk target >> pure True
