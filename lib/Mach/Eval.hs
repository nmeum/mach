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
  let infRule = suffixLookup (suffixes mk) infs
      tgtRule = Map.lookup name targets
   in case tgtRule of
        -- Allow dependencies to be added to inference
        -- rules through target rules with no commands.
        Just tg@(Target _ []) -> (mergeTarget tg <$> infRule) <|> Just tg
        Just tg -> Just tg
        Nothing -> infRule
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
        [ ("^", T.AssignI $ unwords preqs),
          ("@", T.AssignI $ name)
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

lookupAssign :: T.Env -> String -> Maybe String
lookupAssign env name = Map.lookup name env >>= lookupAssign'
  where
    lookupAssign' :: T.MacroAssign -> Maybe String
    lookupAssign' (T.AssignI str) = Just str
    lookupAssign' (T.AssignD tok) = Just $ expand env tok

-- Expand a given macro in the context of a given environment.
expand :: T.Env -> T.Token -> String
expand _ (T.Lit t) = t
expand env (T.Exp t) = fromMaybe "" (lookupAssign env (expand env t))
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

-- A target that has prerequisites, but does not have any commands,
-- can be used to add to the prerequisite list for that target.
mergeTarget :: TgtDef -> TgtDef -> TgtDef
mergeTarget (Target p c) (Target p' c')
  | null c || null c' = Target (p ++ p') (c ++ c')
  | otherwise = error "only one rule for a target can contain commands" -- TODO

mergeTargets :: Map.Map String TgtDef -> Map.Map String TgtDef -> Map.Map String TgtDef
mergeTargets old new =
  (flip Map.union) old $
    Map.mapWithKey
      ( \k v -> case Map.lookup k old of
          Just v' -> mergeTarget v' v
          Nothing -> v
      )
      new

------------------------------------------------------------------------

evalAssign :: T.Env -> T.Assign -> (String, T.MacroAssign)
evalAssign env (T.Assign name ty val) =
  case ty of
    T.Delayed -> (name, T.AssignD val)
    T.Immediate -> (name, T.AssignI $ expand env val)
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
            $ mergeTargets targets newTargets
        )
        xs

-- TODO: Extract command-line environment here.
eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing Map.empty Map.empty)
