module Main where

import Control.Exception (throwIO)
import Mach.Error (MakeErr (..), TargetError (NoSuchTarget, ZeroTargetsDefined))
import Mach.Eval (MkDef, eval, firstTarget, lookupRule)
import Mach.Exec (maybeBuild)
import Mach.Parser (cmdLine, parseMkFile)
import Mach.Types (MkFile)
import Mach.Util (getEnvMarcos)
import Paths_mach (getDataFileName)
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (Permute),
    OptDescr (Option),
    getOpt,
    usageInfo,
  )
import System.Environment (getArgs, getEnv)

data Flag
  = EnvOverwrite
  | Makefile String
  | Jobs String
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['f'] [] (ReqArg Makefile "makefile") "Specify a different makefile",
    Option ['e'] [] (NoArg EnvOverwrite) "Overwrite macro assignments with environment variables",
    Option ['j'] [] (ReqArg Jobs "jobs") "Allow given amount of execution jobs at once"
  ]

makeOpts :: [String] -> IO ([Flag], [String])
makeOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: mach [-f makefile] [-j jobs] [target_name...]"

------------------------------------------------------------------------

makefile :: MkFile -> FilePath -> IO MkDef
makefile extra path = do
  f <- parseMkFile path
  -- TODO: evaluate extra once
  eval (extra ++ f)

runMk :: MkFile -> [String] -> FilePath -> IO ()
runMk extra my_targets path = do
  mk <- makefile extra path
  targets <-
    if null my_targets
      then (: []) <$> firstTarget' mk
      else pure my_targets

  mapM (lookupRule' mk) targets >>= mapM_ (maybeBuild mk)
  where
    firstTarget' mk = case firstTarget mk of
      Nothing -> throwIO $ TargetErr ZeroTargetsDefined
      Just tg -> pure tg

    lookupRule' mk t = do
      tgt <- lookupRule mk t
      case tgt of
        Nothing -> throwIO $ TargetErr (NoSuchTarget t)
        Just tg -> pure tg

main :: IO ()
main = do
  (flags, remain) <- getArgs >>= makeOpts
  (vars, targets) <- cmdLine $ unwords remain

  (_flagsEnv, remainEnv) <- getEnv "MAKEFLAGS" >>= makeOpts . words
  (envMacros, _) <- cmdLine $ unwords remainEnv

  environs <- getEnvMarcos
  builtins <- getDataFileName "share/builtin.mk" >>= parseMkFile

  let extra = builtins ++ vars ++ envMacros ++ environs
  mapM_ (runMk extra targets) $
    case [f | Makefile f <- flags] of
      [] -> ["Makefile"]
      fs -> fs
