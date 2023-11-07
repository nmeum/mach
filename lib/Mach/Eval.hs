-- | This module performs Makefile macro expansion.
module Mach.Eval
  ( TgtDef,
    MkDef,
    getPreqs,
    firstTarget,
    lookupRule,
    getCmds,
    runCmds,
    eval,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, void)
import Data.List (elemIndices, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Mach.Parser (parseMkFile)
import qualified Mach.Types as T
import System.Process (callCommand, createProcess, shell, waitForProcess)

-- Expanded commands of a target.
newtype Cmds = Cmds [String]

-- TODO: Consider expanding this, also tracking the name and the
-- type of the rule here (e.g. for inference rules). Useful for
-- the implementation of internal macros.
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
      -- | Inference rules defined in this Makefile.
      (Map.Map String TgtDef)
      -- | Targets defined in this Makefile.
      (Map.Map String TgtDef)
  deriving (Show)

------------------------------------------------------------------------

-- | Obtain the first target that is not a special target or an
-- inference rule. 'Nothing' if the Makefile defines no targets.
firstTarget :: MkDef -> Maybe String
firstTarget (MkDef _ fstTarget _ _) = fstTarget

-- | Return all suffixes.
--
-- TODO: Use .SUFFIXES for this purpose.
suffixes :: MkDef -> [String]
suffixes (MkDef _ _ infs _) = Map.keys infs

-- | Lookup either a target or inference rule.
lookupRule :: MkDef -> String -> Maybe TgtDef
lookupRule mk@(MkDef _ _ infs targets) name =
  Map.lookup name targets
    <|> suffixLookup (suffixes mk) infs
  where
    suffixLookup :: [String] -> Map.Map String TgtDef -> Maybe TgtDef
    suffixLookup [] _ = Nothing
    suffixLookup (ruleName : xs) infRules =
      let (src, tgt) = getSuffixes ruleName
       in if tgt `isSuffixOf` name
            then
              (\(Target _ cmds) -> Target [stripSuffix name ++ src] cmds)
                <$> Map.lookup ruleName infRules
            else suffixLookup xs infRules

    stripSuffix :: String -> String
    stripSuffix = fst . getSuffixes

    -- For a string of the form `.s2.s1` return `(".s2", ".s1")`.
    --
    -- TODO: Ensure that the string only contains two period characters.
    getSuffixes :: String -> (String, String)
    getSuffixes str = splitAt (last $ elemIndices '.' str) str

getPreqs :: TgtDef -> [String]
getPreqs (Target preqs _) = preqs

getCmds :: MkDef -> String -> TgtDef -> Cmds
getCmds (MkDef env _ _ _) name (Target preqs cmds) =
  Cmds $ fmap (expand $ Map.union internalMacros env) cmds
  where
    internalMacros :: T.Env
    internalMacros =
      Map.fromList
        [ ("^", unwords preqs),
          ("@", name)
        ]

runCmds :: Cmds -> IO ()
runCmds (Cmds cmds) = mapM_ runCmd cmds
  where
    -- TODO: Parse prefixes in Parser
    runCmd :: String -> IO ()
    runCmd ('-' : cmd) = do
      (_, _, _, p) <- createProcess (shell cmd)
      void $ waitForProcess p
    runCmd ('@' : cmd) = callCommand cmd
    runCmd cmd = putStrLn cmd >> callCommand cmd

-- Expand a given macro in the context of a given environment.
expand :: T.Env -> T.Token -> String
expand _ (T.Lit t) = t
expand env (T.Exp t) = fromMaybe "" (Map.lookup (expand env t) env)
expand env (T.Seq s) = foldr (\x acc -> expand env x ++ acc) "" s
expand env (T.ExpSub t s1 s2) =
  unwords $
    map
      (subWord s1 s2)
      (words $ expand env t)
  where
    subWord :: String -> String -> String -> String
    subWord s r w
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

evalTgtRule :: T.Env -> T.TgtRule -> Map.Map String TgtDef
evalTgtRule env (T.TgtRule tgts preqs cmds) =
  let def = Target (exLst preqs) cmds
   in Map.fromList $ map (\tgt -> (tgt, def)) (exLst tgts)
  where
    exLst = concatMap (words . expand env)

eval' :: MkDef -> T.MkFile -> IO MkDef
eval' def [] = pure def
eval' (MkDef env fstTgt infs targets) ((T.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval' (MkDef newEnviron fstTgt infs targets) xs
eval' def@(MkDef env _ _ _) ((T.MkInclude elems) : xs) = do
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
eval' (MkDef env fstTgt infs targets) ((T.MkInfRule (T.InfRule target cmds)) : xs) =
  let def = Target [] cmds
   in eval' (MkDef env fstTgt (Map.insert target def infs) targets) xs
eval' (MkDef env fstTgt infs targets) ((T.MkTgtRule rule) : xs) =
  let newTargets = evalTgtRule env rule
      initTarget = head $ Map.keys newTargets
   in eval'
        ( MkDef
            env
            (fstTgt <|> Just initTarget)
            infs
            $ Map.union newTargets targets
        )
        xs

-- TODO: Extract command-line environment here.
eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing Map.empty Map.empty)
