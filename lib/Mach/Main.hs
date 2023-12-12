module Mach.Main (run) where

import Control.Exception (throwIO)
import Mach.Error (MakeErr (..), TargetError (NoSuchTarget, ZeroTargetsDefined))
import Mach.Eval (MkDef, eval, firstTarget)
import Mach.Exec (maybeBuild, mkConfig, targetOrFile)
import Mach.Parser (cmdLine, parseMkFile)
import qualified Mach.Types as T
import Mach.Util (getEnvMarcos)
import Paths_mach (getDataFileName)
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (Permute),
    OptDescr (Option),
    getOpt,
    usageInfo,
  )
import System.Environment (getEnv)
import System.IO (Handle)

options :: [OptDescr T.Flag]
options =
  [ Option ['f'] [] (ReqArg T.Makefile "makefile") "Specify a different makefile",
    Option ['e'] [] (NoArg T.EnvOverwrite) "Overwrite macro assignments with environment variables",
    Option ['j'] [] (ReqArg T.Jobs "jobs") "Allow given amount of execution jobs at once"
  ]

makeOpts :: [String] -> IO ([T.Flag], [String])
makeOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: mach [-f makefile] [-j jobs] [target_name...]"

------------------------------------------------------------------------

makefile :: [T.Flag] -> T.MkFile -> T.MkFile -> FilePath -> IO MkDef
makefile my_flags extra environ path = do
  f <- parseMkFile path

  -- If -e is specified, overwrite macro assignments with environment.
  let mk =
        if null [f | T.EnvOverwrite <- my_flags]
          then extra ++ environ ++ f
          else extra ++ f ++ environ

  -- TODO: evaluate extra and environ once
  eval (mk)

runMk :: Handle -> [T.Flag] -> T.MkFile -> T.MkFile -> [String] -> FilePath -> IO ()
runMk handle my_flags extra environ my_targets path = do
  mk <- makefile my_flags extra environ path
  targets <-
    if null my_targets
      then (: []) <$> firstTarget' mk
      else pure my_targets

  let conf = mkConfig mk handle my_flags

  mapM (targetOrFile' mk) targets >>= mapM_ (maybeBuild conf mk)
  where
    firstTarget' mk = case firstTarget mk of
      Nothing -> throwIO $ TargetErr ZeroTargetsDefined
      Just tg -> pure tg

    targetOrFile' mk t = do
      tgt <- targetOrFile mk t
      case tgt of
        Nothing -> throwIO $ TargetErr (NoSuchTarget t)
        Just tg -> pure tg

run :: Handle -> [String] -> IO ()
run handle args = do
  (flagsCmd, remain) <- makeOpts args
  (vars, targets) <- cmdLine $ unwords remain

  (flagsEnv, remainEnv) <- getEnv "MAKEFLAGS" >>= makeOpts . words
  (envMacros, _) <- cmdLine $ unwords remainEnv

  environs <- getEnvMarcos
  builtins <- getDataFileName "share/builtin.mk" >>= parseMkFile

  let extra = builtins ++ vars ++ envMacros
  mapM_ (runMk handle (flagsCmd ++ flagsEnv) extra environs targets) $
    case [f | T.Makefile f <- flagsCmd] of
      [] -> ["Makefile"]
      fs -> fs
