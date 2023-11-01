module Mach.Eval (TgtDef (..), MkDef (..), getCmds, eval) where

import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Mach.Types as T

data TgtDef
  = Target
      -- | Prerequisites (expanded)
      (Seq.Seq String)
      -- | Commands
      (Seq.Seq T.Token)
  deriving (Show)

data MkDef = MkDef T.Env (Map.Map String TgtDef)
  deriving (Show)

------------------------------------------------------------------------

-- TODO: Utilize information hiding to prevent direct commands access.
getCmds :: MkDef -> TgtDef -> [String]
getCmds (MkDef env _) (Target _ cmds) =
  toList $ fmap (expand env) cmds

-- Expand a given macro in the context of a given environment.
expand :: T.Env -> T.Token -> String
expand _ (T.Lit t) = t
expand env (T.Exp t) = fromMaybe "" (Map.lookup (expand env t) env)
expand env (T.Seq s) = foldr (\x acc -> expand env x ++ acc) "" s

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
  let def = Target (exSeq preqs) cmds
   in Map.fromList $ toList $ fmap (\tgt -> (tgt, def)) (exSeq tgts)
  where
    exSeq :: Seq.Seq T.Token -> Seq.Seq String
    exSeq = fmap (expand env)

eval' :: MkDef -> T.MkFile -> MkDef
eval' def [] = def
eval' (MkDef env targets) ((T.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval' (MkDef newEnviron targets) xs
eval' _ ((T.MkInclude _paths) : _xs) =
  -- TODO:
  --  • Parse each path element as a Makefile
  --  • Evaluate each parsed Makefile recursively
  --  • Create a new MkDef value
  error "include support not yet implemented"
eval' (MkDef env targets) ((T.MkRule rule) : xs) =
  let newTargets = Map.union (evalRule env rule) targets
   in eval' (MkDef env newTargets) xs

-- TODO: Extract command-line environment here.
eval :: T.MkFile -> MkDef
eval = eval' $ MkDef Map.empty Map.empty
