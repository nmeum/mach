module Main where

import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Maybe (fromJust)
import Mach.Error (MakeErr (..), TargetError (ZeroTargetsDefined))
import Mach.Eval (MkDef, eval, firstTarget)
import Mach.Exec (lookupTarget, maybeBuild)
import Mach.Parser (parseMkFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

makefile :: FilePath -> IO MkDef
makefile path = parseMkFile path >>= eval

runMk :: FilePath -> IO ()
runMk path = do
  mk <- makefile path
  case firstTarget mk of
    Nothing -> throwIO $ TargetErr ZeroTargetsDefined
    Just tg -> void $ maybeBuild mk (fromJust $ lookupTarget mk tg)
  pure ()

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then hPutStrLn stderr "Missing file argument"
    else runMk $ head args
