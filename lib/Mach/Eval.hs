-- | This module performs Makefile macro expansion.
module Mach.Eval
  ( TgtDef,
    MkDef,
    getPreqs,
    firstTarget,
    lookupRule,
    getCmds,
    eval,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.List (isSuffixOf)
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

data MkDef
  = MkDef
      -- | Macros defined in this Makefile.
      T.Env
      -- | First "normal" target defined in the Makefile.
      (Maybe String)
      -- | Targets defined in this Makefile.
      (Map.Map String TgtDef)
  deriving (Show)

------------------------------------------------------------------------

-- | Obtain the first target that is not a special target or an
-- inference rule. 'Nothing' if the Makefile defines no targets.
firstTarget :: MkDef -> Maybe String
firstTarget (MkDef _ fstTarget _) = fstTarget

lookupRule :: MkDef -> String -> Maybe TgtDef
lookupRule (MkDef _ _ targets) = flip Map.lookup targets

getPreqs :: TgtDef -> [String]
getPreqs (Target preqs _) = preqs

getCmds :: MkDef -> TgtDef -> [String]
getCmds (MkDef env _ _) (Target _ cmds) =
  fmap (expand env) cmds

-- Expand a given macro in the context of a given environment.
expand :: T.Env -> T.Token -> String
expand _ (T.Lit t) = t
expand env (T.Exp t) = fromMaybe "" (Map.lookup (expand env t) env)
expand env (T.Seq s) = foldr (\x acc -> expand env x ++ acc) "" s
expand env (T.ExpSub t s1 s2) =
  foldl
    ( \acc word ->
        subWord word s1 s2 ++ acc
    )
    ""
    (words $ expand env t)
  where
    subWord :: String -> String -> String -> String
    subWord w s r
      | s `isSuffixOf` w = take (length w - length s) w ++ r
      | otherwise = w

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

eval' :: MkDef -> T.MkFile -> IO MkDef
eval' def [] = pure def
eval' (MkDef env fstTgt targets) ((T.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval' (MkDef newEnviron fstTgt targets) xs
eval' def@(MkDef env _ _) ((T.MkInclude elems) : xs) = do
  let paths = map (expand env) elems
  included <-
    foldM
      ( \mkDef path -> do
          mk <- parseMkFile path
          eval' mkDef mk
      )
      def
      paths

  eval' included xs
eval' (MkDef env fstTarget targets) ((T.MkRule rule) : xs) =
  let newTargets = evalRule env rule
      initTarget = head $ Map.keys newTargets
   in eval'
        ( MkDef env (fstTarget <|> Just initTarget) $
            Map.union newTargets targets
        )
        xs

-- TODO: Extract command-line environment here.
eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing Map.empty)
