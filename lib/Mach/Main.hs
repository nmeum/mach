module Mach.Main (run) where

import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
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
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle)

options :: [OptDescr T.Flag]
options =
  [ Option ['f'] [] (ReqArg T.Makefile "makefile") "Specify a different makefile",
    Option ['e'] [] (NoArg T.EnvOverwrite) "Overwrite macro assignments with environment variables",
    Option ['j'] [] (ReqArg T.Jobs "jobs") "Allow given amount of execution jobs at once",
    Option ['i'] [] (NoArg T.IgnoreAll) "Ignore exit status of executed commands",
    Option ['s'] [] (NoArg T.SilentAll) "Do not write command lines to stdout",
    Option ['k'] [] (NoArg T.ExecCont) "On error keep executing independent targets",
    Option ['n'] [] (NoArg T.DryRun) "Write commands to be executed to stdout",
    Option ['S'] [] (NoArg T.TermOnErr) "Terminate on error (the default)",
    Option ['r'] [] (NoArg T.NoBuiltin) "Do not use the builtin rules"
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
        if null [() | T.EnvOverwrite <- my_flags]
          then extra ++ environ ++ f
          else extra ++ f ++ environ

  -- TODO: evaluate extra and environ once
  eval (mk)

runMk :: Handle -> [T.Flag] -> T.MkFile -> T.MkFile -> [String] -> FilePath -> IO Bool
runMk handle my_flags extra environ my_targets path = do
  mk <- makefile my_flags extra environ path
  targets <-
    if null my_targets
      then (: []) <$> firstTarget' mk
      else pure my_targets

  putStrLn $ show mk

  let conf = mkConfig mk handle my_flags
  (not . any (== False))
    <$> (mapM (targetOrFile' mk) targets >>= mapM (maybeBuild conf mk))
  where
    firstTarget' mk = case firstTarget mk of
      Nothing -> throwIO $ TargetErr ZeroTargetsDefined
      Just tg -> pure tg

    targetOrFile' mk t = do
      tgt <- targetOrFile mk t
      case tgt of
        Nothing -> throwIO $ TargetErr (NoSuchTarget t)
        Just tg -> pure tg

run :: Handle -> [String] -> IO ExitCode
run handle args = do
  (flagsCmd, remain) <- makeOpts args
  (vars, targets) <- cmdLine $ unwords remain

  (flagsEnv, remainEnv) <- (fromMaybe "" <$> lookupEnv "MAKEFLAGS") >>= makeOpts . words
  (envMacros, _) <- cmdLine $ unwords remainEnv

  environs <- getEnvMarcos
  builtins <- getDataFileName "share/builtin.mk" >>= parseMkFile

  let my_flags = flagsCmd ++ flagsEnv
  let extra =
        if null [() | T.NoBuiltin <- my_flags]
          then builtins ++ vars ++ envMacros
          else vars ++ envMacros

  res <- mapM (runMk handle my_flags extra environs targets) $
    case [f | T.Makefile f <- flagsCmd] of
      [] -> ["Makefile"]
      fs -> fs

  pure $
    if (any (== False) res)
      then ExitFailure 1
      else ExitSuccess
