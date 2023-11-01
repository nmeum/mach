-- | This module performs Makefile macro expansion.
module Mach.Eval (TgtDef (..), MkDef (..), getCmds, firstTarget, eval) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Mach.Parser (parseMkFile)
import qualified Mach.Types as T

data TgtDef
  = Target
      -- | Prerequisites (expanded)
      [String]
      -- | Commands
      [T.Token]
  deriving (Show)

data MkDef = MkDef T.Env (Maybe String) (Map.Map String TgtDef)
  deriving (Show)

firstTarget :: MkDef -> Maybe String
firstTarget (MkDef _ fstTarget _) = fstTarget

------------------------------------------------------------------------

-- TODO: Utilize information hiding to prevent direct commands access.
getCmds :: MkDef -> TgtDef -> [String]
getCmds (MkDef env _ _) (Target _ cmds) =
  fmap (expand env) cmds

-- Expand a given macro in the context of a given environment.
expand :: T.Env -> T.Token -> String
expand _ (T.Lit t) = t
expand env (T.Exp t) = fromMaybe "" (Map.lookup (expand env t) env)
expand env (T.Seq s) = foldr (\x acc -> expand env x ++ acc) "" s

------------------------------------------------------------------------

evalAssign :: T.Env -> T.Assign -> (String, String)
evalAssign env (T.Assign name ty val) =
  case ty of
    T.Delayed -> error "unsupported"
    T.Immediate -> (name, expand env val)
    T.StrictDelay -> error "unsupported"
    T.System -> error "unsupported"
    T.Cond -> error "unsupported"
    T.Append -> error "unsupported"

evalRule :: T.Env -> T.Rule -> Map.Map String TgtDef
evalRule env (T.Rule tgts preqs cmds) =
  let def = Target (exLst preqs) cmds
   in Map.fromList $ map (\tgt -> (tgt, def)) (exLst tgts)
  where
    exLst = map (expand env)

eval'' :: MkDef -> T.MkFile -> IO MkDef
eval'' def [] = pure def
eval'' (MkDef env fstTgt targets) ((T.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval'' (MkDef newEnviron fstTgt targets) xs
eval'' def@(MkDef env _ _) ((T.MkInclude elems) : _xs) = do
  let paths = map (expand env) elems
  foldM
    ( \mkDef path -> do
        mk <- parseMkFile path
        eval' mkDef mk
    )
    def
    paths
eval'' (MkDef env _ targets) ((T.MkRule rule) : xs) =
  let newTargets = evalRule env rule
      initTarget = head $ Map.keys newTargets
   in eval'' (MkDef env (Just initTarget) $ Map.union newTargets targets) xs

-- TODO: Don't not reverse here (needed to find first target)
eval' :: MkDef -> T.MkFile -> IO MkDef
eval' mkDef mkFile = eval'' mkDef $ reverse mkFile

-- TODO: Extract command-line environment here.
eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing Map.empty)
