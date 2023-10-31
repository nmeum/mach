module Mach.Eval (TgtDef (..), MkDef (..), eval) where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Mach.Macro (Assign (..), Env, Flavor (..), Token (..), expand)
import qualified Mach.Parser as P

data TgtDef
  = Target
      -- | Prerequisites (expanded)
      (Seq.Seq String)
      -- | Commands
      (Seq.Seq Token)
  deriving (Show)

data MkDef = MkDef Env (Map.Map String TgtDef)
  deriving (Show)

------------------------------------------------------------------------

evalAssign :: Env -> Assign -> (String, String)
evalAssign env (Assign name ty val) =
  case ty of
    Delayed -> error "unsupported"
    Immediate -> (name, expand env val)
    StrictDelay -> error "unsupported"
    System -> error "unsupported"
    Cond -> error "unsupported"
    Append -> error "unsupported"

evalRule :: Env -> P.Rule -> Map.Map String TgtDef
evalRule env (P.Rule tgts preqs cmds) =
  let def = Target (exSeq preqs) cmds
   in Map.fromList $ toList $ fmap (\tgt -> (tgt, def)) (exSeq tgts)
  where
    exSeq :: Seq.Seq Token -> Seq.Seq String
    exSeq = fmap (expand env)

eval' :: MkDef -> P.MkFile -> MkDef
eval' def [] = def
eval' (MkDef env targets) ((P.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval' (MkDef newEnviron targets) xs
eval' _ ((P.MkInclude _paths) : _xs) =
  -- TODO:
  --  • Parse each path element as a Makefile
  --  • Evaluate each parsed Makefile recursively
  --  • Create a new MkDef value
  error "include support not yet implemented"
eval' (MkDef env targets) ((P.MkRule rule) : xs) =
  let newTargets = Map.union (evalRule env rule) targets
   in eval' (MkDef env newTargets) xs

-- TODO: Extract command-line environment here.
eval :: P.MkFile -> MkDef
eval = eval' $ MkDef Map.empty Map.empty
