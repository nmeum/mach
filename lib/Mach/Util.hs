{-# LANGUAGE LambdaCase #-}

module Mach.Util where

import Data.List (elemIndices)
import qualified Mach.Types as T
import System.Environment (getEnvironment)

-- Strip a file name extension (suffix) from a file name.
stripSuffix :: FilePath -> String
stripSuffix name =
  let indices = elemIndices '.' name
   in if null indices
        then name
        else take (last indices) name

-- | Like 'find', but allows the predicate to run in a monadic context
-- and, furthermore, enables the predicate to compute some additional
-- information.
--
-- Inspired by https://hackage.haskell.org/package/extra-1.7.14/docs/Control-Monad-Extra.html#v:firstJustM
firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM p (x : xs) = do
  p x >>= \case
    Just y -> pure $ Just y
    Nothing -> firstJustM p xs

-- Returns true if the target name is a special target.
isSpecial :: String -> Bool
isSpecial = flip elem special
  where
    special :: [String]
    special =
      [ ".DEFAULT",
        ".IGNORE",
        ".PHONY",
        ".NOTPARALLEL",
        ".POSIX",
        ".PRECIOUS",
        ".SILENT",
        ".SUFFIXES",
        ".WAIT"
      ]

-- Returns environment variables as macro assignments.
getEnvMarcos :: IO T.MkFile
getEnvMarcos = do
  env <- filter ((not . flip elem excluded) . fst) <$> getEnvironment
  pure $ map (\(k, v) -> T.MkAssign $ T.Assign (T.Lit k) T.Immediate (T.Lit v)) env
  where
    excluded :: [String]
    excluded = ["SHELL", "MAKEFLAGS"]
