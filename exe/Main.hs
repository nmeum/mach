module Main where

import Control.Exception (throwIO)
import Mach.Error (MakeErr (..), TargetError (NoSuchTarget, ZeroTargetsDefined))
import Mach.Eval (MkDef, eval, firstTarget)
import Mach.Exec (lookupTarget, maybeBuild)
import Mach.Parser (parseMkFile)
import Mach.Types (MkFile)
import Paths_mach (getDataFileName)
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

makefile :: MkFile -> FilePath -> IO MkDef
makefile builtins path = do
  f <- parseMkFile path
  -- TODO: evaluate builtins once
  eval (builtins ++ f)

runMk :: MkFile -> [String] -> FilePath -> IO ()
runMk builtins my_targets path = do
  mk <- makefile builtins path
  targets <-
    if null my_targets
      then (: []) <$> firstTarget' mk
      else pure my_targets

  mapM (lookupTarget' mk) targets >>= mapM_ (maybeBuild mk)
  where
    firstTarget' mk = case firstTarget mk of
      Nothing -> throwIO $ TargetErr ZeroTargetsDefined
      Just tg -> pure tg

    lookupTarget' mk t = do
      tgt <- lookupTarget mk t
      case tgt of
        Nothing -> throwIO $ TargetErr (NoSuchTarget t)
        Just tg -> pure tg

main :: IO ()
main = do
  -- TODO: targets is all remaining non-flag arguments,
  -- it may contain key=value pairs which must be parsed.
  (flags, targets) <- getArgs >>= makeOpts

  -- Builtin macros as mandated by POSIX
  builtins <- getDataFileName "share/builtin.mk" >>= parseMkFile

  mapM_ (runMk builtins targets) $
    case [f | Makefile f <- flags] of
      [] -> ["Makefile"]
      fs -> fs
