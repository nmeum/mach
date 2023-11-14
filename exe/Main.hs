module Main where

import Control.Exception (throwIO)
import Data.Maybe (fromJust)
import Mach.Error (MakeErr (..), TargetError (ZeroTargetsDefined))
import Mach.Eval (MkDef, eval, firstTarget)
import Mach.Exec (lookupTarget, maybeBuild)
import Mach.Parser (parseMkFile)
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (Permute),
    OptDescr (Option),
    getOpt,
    usageInfo,
  )
import System.Environment (getArgs)

data Flag
  = EnvOverwrite
  | Makefile String
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['f'] [] (ReqArg Makefile "makefile") "Specify a different makefile",
    Option ['e'] [] (NoArg EnvOverwrite) "Overwrite macro assignments with environment variables"
  ]

makeOpts :: [String] -> IO ([Flag], [String])
makeOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: mach [-f makefile] [target_name...]"

------------------------------------------------------------------------

makefile :: FilePath -> IO MkDef
makefile path = parseMkFile path >>= eval

runMk :: FilePath -> IO ()
runMk path = do
  mk <- makefile path
  case firstTarget mk of
    Nothing -> throwIO $ TargetErr ZeroTargetsDefined
    Just tg -> maybeBuild mk (fromJust $ lookupTarget mk tg)
  pure ()

main :: IO ()
main = do
  (flags, _remain) <- getArgs >>= makeOpts
  mapM_ runMk $
    case [f | Makefile f <- flags] of
      [] -> ["Makefile"]
      fs -> fs
