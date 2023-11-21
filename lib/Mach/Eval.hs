{-# LANGUAGE LambdaCase #-}

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
import Control.Exception (throwIO)
import Control.Monad (foldM, void)
import Data.Functor ((<&>))
import Data.List (elemIndices, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Mach.Error (MakeErr (TargetErr), TargetError (MultipleDefines))
import Mach.Parser (parseMkFile)
import qualified Mach.Types as T
import System.Directory (doesPathExist)
import System.Process (callCommand, createProcess, shell, waitForProcess)

----
-- TODO: Move FileTarget here and also add the getSrc field to it
--
-- Seperate constructors for file and inference targets
----

-- | Expanded target definition of a target rule or inference rule.
data TgtDef = TgtDef
  { -- | Prerequisites (expanded)
    getPreqs :: [String],
    -- | Source file name for inferred targets.
    getSrc :: Maybe String,
    -- | Commands
    getCmds' :: [T.Token]
  }
  deriving (Show)

-- | Expanded makefile definition.
data MkDef
  = MkDef
      -- | Macros defined in this Makefile.
      Env
      -- | First "normal" target defined in the Makefile.
      (Maybe String)
      -- | Inference rules defined in this Makefile.
      (Map.Map String TgtDef)
      -- | TgtDefs defined in this Makefile.
      (Map.Map String TgtDef)
  deriving (Show)

-- | Obtain the name of the first defined target that is not a special
-- target or an inference rule. 'Nothing' if the Makefile defines no
-- target rules.
firstTarget :: MkDef -> Maybe String
firstTarget (MkDef _ fstTarget _ _) = fstTarget

-- | Return all suffixes.
--
-- TODO: Use .SUFFIXES for this purpose.
suffixes :: MkDef -> [String]
suffixes (MkDef _ _ infs _) = Map.keys infs

-- | TODO
stripSuffix :: String -> String
stripSuffix = fst . getSuffixes

-- For a string of the form `.s2.s1` return `(".s2", ".s1")`.
--
-- TODO: Ensure that the string only contains two period characters.
getSuffixes :: String -> (String, String)
getSuffixes str = splitAt (last $ elemIndices '.' str) str

-- | Lookup a target definition, the definition may be inferred.
--
-- TODO: Refactor this
lookupRule :: MkDef -> String -> IO (Maybe TgtDef)
lookupRule mk@(MkDef _ _ infs targets) name =
  lookupRule' $ Map.lookup name targets
  where
    lookupRule' :: Maybe TgtDef -> IO (Maybe TgtDef)
    lookupRule' Nothing = infRule
    lookupRule' (Just t)
      | null (getCmds' t) =
          infRule <&> \case
            Nothing -> Just t
            -- mergeDef will never return Nothing as
            -- inference rules do not have any prerqs.
            Just x -> mergeDef x t
      | otherwise = pure $ Just t

    infRule :: IO (Maybe TgtDef)
    infRule = doubleSuffix name (suffixes mk) infs

doubleSuffix :: String -> [String] -> Map.Map String TgtDef -> IO (Maybe TgtDef)
doubleSuffix _ [] _ = pure Nothing
doubleSuffix name (ruleName : xs) infRules = do
  let (src, tgt) = getSuffixes ruleName
  let fnameNoExt = stripSuffix name

  let srcName = fnameNoExt ++ src
  srcExists <- doesPathExist srcName
  if tgt `isSuffixOf` name && srcExists
    then do
      let cmds = getCmds' <$> Map.lookup ruleName infRules
      pure (TgtDef [fnameNoExt ++ src] (Just srcName) <$> cmds)
    else doubleSuffix name xs infRules

-- A target that has prerequisites, but does not have any commands,
-- can be used to add to the prerequisite list for that target.
-- Returns nothing if the targets cannot be merged, i.e. if both
-- targets define commands.
mergeDef :: TgtDef -> TgtDef -> Maybe TgtDef
mergeDef
  TgtDef {getPreqs = p, getSrc = s, getCmds' = c}
  TgtDef {getPreqs = p', getSrc = s', getCmds' = c'}
    | null c || null c' = Just $ TgtDef (p ++ p') (s <|> s') (c ++ c')
    | otherwise = Nothing

mergeDefs :: Map.Map String TgtDef -> Map.Map String TgtDef -> Maybe (Map.Map String TgtDef)
mergeDefs old new =
  flip Map.union old
    <$> Map.traverseWithKey
      ( \k v -> case Map.lookup k old of
          Just v' -> mergeDef v' v
          Nothing -> Just v
      )
      new

------------------------------------------------------------------------

-- | Expanded commands of a target.
newtype Cmds = Cmds [String]

getCmds :: MkDef -> String -> TgtDef -> Cmds
getCmds (MkDef env _ _ _) name target =
  Cmds $ fmap (expand $ Map.union internalMacros env) (getCmds' target)
  where
    internalMacros :: Env
    internalMacros =
      Map.fromList
        [ ("^", T.AssignI $ unwords (getPreqs target)),
          ("@", T.AssignI name),
          ("<", T.AssignI $ fromMaybe "" (getSrc target))
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

------------------------------------------------------------------------

-- | Makefile environment consisting of macro definitions.
type Env = Map.Map String T.MacroAssign

lookupAssign :: Env -> String -> Maybe String
lookupAssign env name = Map.lookup name env >>= lookupAssign'
  where
    lookupAssign' :: T.MacroAssign -> Maybe String
    lookupAssign' (T.AssignI str) = Just str
    lookupAssign' (T.AssignD tok) = Just $ expand env tok

-- Expand a given macro in the context of a given environment.
expand :: Env -> T.Token -> String
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

------------------------------------------------------------------------

evalAssign :: Env -> T.Assign -> (String, T.MacroAssign)
evalAssign env (T.Assign name ty val) =
  case ty of
    T.Delayed -> (name, T.AssignD val)
    T.Immediate -> (name, T.AssignI $ expand env val)
    T.StrictDelay -> error "unsupported"
    T.System -> error "unsupported"
    T.Cond -> error "unsupported"
    T.Append -> error "unsupported"

evalInclude :: MkDef -> [T.Token] -> IO MkDef
evalInclude def@(MkDef env _ _ _) elems =
  foldM
    ( \mkDef path -> do
        mk <- parseMkFile path
        eval' mkDef mk
    )
    def
    $ map (expand env) elems

evalTgtRule :: Env -> T.TgtRule -> Map.Map String TgtDef
evalTgtRule env (T.TgtRule tgts preqs cmds) =
  let def = TgtDef (exLst preqs) Nothing cmds
   in Map.fromList $ map (\tgt -> (tgt, def)) (exLst tgts)
  where
    exLst = concatMap (words . expand env)

eval' :: MkDef -> T.MkFile -> IO MkDef
eval' def [] = pure def
eval' (MkDef env fstTgt infs targets) ((T.MkAssign assign) : xs) =
  let (key, val) = evalAssign env assign
      newEnviron = Map.insert key val env
   in eval' (MkDef newEnviron fstTgt infs targets) xs
eval' def ((T.MkInclude elems) : xs) =
  evalInclude def elems >>= flip eval' xs
eval' (MkDef env fstTgt infs targets) ((T.MkInfRule (T.InfRule target cmds)) : xs) =
  let def = TgtDef [] Nothing cmds
   in eval' (MkDef env fstTgt (Map.insert target def infs) targets) xs
eval' (MkDef env fstTgt infs targets) ((T.MkTgtRule rule) : xs) =
  let newTgtDefs = evalTgtRule env rule
      initTgtDef = head $ Map.keys newTgtDefs
   in case mergeDefs targets newTgtDefs of
        Nothing -> throwIO $ TargetErr MultipleDefines
        Just nt -> eval' (MkDef env (fstTgt <|> Just initTgtDef) infs nt) xs

eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing Map.empty Map.empty)
