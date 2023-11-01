module Main where

import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Maybe (fromJust)
import Mach.Error (MakeErr (..), TargetError (ZeroTargetsDefined))
import Mach.Exec (lookupTarget, maybeBuild)
import Mach.Parser (makefile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

runMk :: FilePath -> IO ()
runMk path = do
  (mk, firstTarget) <- makefile path
  case firstTarget of
    Nothing -> throwIO $ TargetErr ZeroTargetsDefined
    Just tg -> void $ maybeBuild mk (fromJust $ lookupTarget mk tg)
  pure ()

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then hPutStrLn stderr "Missing file argument"
    else runMk $ head args
