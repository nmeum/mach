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
    Target,
    getName,
    getDef,
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

-- | Expanded target definition of a target rule or inference rule.
-- The same target definition may be used for multiple files. For
-- example, when a target rule is defined for multiple targets.
--
-- Refer to 'Target' for more information.
data TgtDef = TgtDef
  { -- | Prerequisites (expanded)
    getPreqs :: [String],
    -- | Commands
    getRawCmds :: [T.Token]
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
      [(String, TgtDef)]
      -- | TgtDefs defined in this Makefile.
      (Map.Map String TgtDef)
  deriving (Show)

-- | Obtain the name of the first defined target that is not a special
-- target or an inference rule. 'Nothing' if the Makefile defines no
-- target rules.
firstTarget :: MkDef -> Maybe String
firstTarget (MkDef _ fstTarget _ _) = fstTarget

-- | Return all suffixes.
suffixes :: MkDef -> [String]
suffixes (MkDef _ _ _ targets) =
  maybe [] getPreqs (Map.lookup ".SUFFIXES" targets)

stripSuffix :: String -> String
stripSuffix name =
  let indices = elemIndices '.' name
   in if null indices
        then name
        else take (last indices) name

-- | Lookup a target definition, the definition may be inferred.
--
-- TODO: Refactor this
lookupRule :: MkDef -> String -> IO (Maybe Target)
lookupRule mk@(MkDef _ _ infs targets) name = lookupRule' $ Map.lookup name targets
  where
    lookupRule' :: Maybe TgtDef -> IO (Maybe Target)
    lookupRule' Nothing = infRule
    lookupRule' (Just t)
      | null (getRawCmds t) =
          infRule <&> \case
            Nothing -> Just (Target name t)
            -- mergeDef will never return Nothing as
            -- inference rules do not have any prerqs.
            Just x -> setDef x <$> mergeDef (getDef x) t
      | otherwise = pure $ Just (Target name t)

    infRule :: IO (Maybe Target)
    infRule = suffixLookup name (suffixes mk) infs

-- Inspired by https://hackage.haskell.org/package/extra-1.7.14/docs/Control-Monad-Extra.html#v:firstJustM
firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM p (x : xs) = do
  p x >>= \case
    Just y -> pure $ Just y
    Nothing -> firstJustM p xs

suffixLookup :: String -> [String] -> [(String, TgtDef)] -> IO (Maybe Target)
suffixLookup _ [] _ = pure Nothing
suffixLookup name (suffix : xs) infRules = do
  target <-
    if '.' `elem` name
      then firstJustM (lookupDouble name) (filter (\(x, _) -> (length (elemIndices '.' x) == 2)) infRules)
      else firstJustM (lookupSingle name) (filter (\(x, _) -> (length (elemIndices '.' x) == 1)) infRules)

  case target of
    Nothing -> suffixLookup name xs infRules
    result -> pure result
  where
    maybeTarget :: TgtDef -> FilePath -> FilePath -> IO (Maybe Target)
    maybeTarget tgtDef tgtName srcName = do
      srcExists <- doesPathExist srcName
      pure $
        if srcExists
          then Just $ Inferred tgtName srcName (TgtDef [srcName] $ getRawCmds tgtDef)
          else Nothing

    lookupSingle :: String -> (String, TgtDef) -> IO (Maybe Target)
    lookupSingle tgtName (ruleName, tgtDef) =
      if ruleName == suffix
        then maybeTarget tgtDef tgtName (tgtName ++ suffix)
        else pure Nothing

    lookupDouble :: String -> (String, TgtDef) -> IO (Maybe Target)
    lookupDouble tgtName (ruleName, tgtDef) =
      let indices = elemIndices '.' ruleName
          baseName = stripSuffix tgtName
          (src, tgt) = splitAt (last indices) ruleName
       in if suffix `isSuffixOf` name && suffix `isSuffixOf` ruleName
            then maybeTarget tgtDef (baseName ++ tgt) (baseName ++ src)
            else pure Nothing

-- A target that has prerequisites, but does not have any commands,
-- can be used to add to the prerequisite list for that target.
-- Returns nothing if the targets cannot be merged, i.e. if both
-- targets define commands.
mergeDef :: TgtDef -> TgtDef -> Maybe TgtDef
mergeDef (TgtDef p c) (TgtDef p' c')
  | null c || null c' = Just $ TgtDef (p ++ p') (c ++ c')
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

-- | A 'TgtDef' initialized for a specific target. The target
-- can either be build from a target rule or an inference rule.
data Target
  = -- | A target build from a target rule
    Target
      -- | The target name
      FilePath
      -- | The target definition
      TgtDef
  | -- | A target build from an inference rule
    Inferred
      -- | The target name
      FilePath
      -- | The source file name (`$<`)
      FilePath
      -- | The target definition
      TgtDef
  deriving (Show)

-- | Obtain the target name.
getName :: Target -> String
getName (Target name _) = name
getName (Inferred name _ _) = name

-- | Obtain the target definition, see 'TgtDef'.
getDef :: Target -> TgtDef
getDef (Target _ def) = def
getDef (Inferred _ _ def) = def

-- | Create a new 'Target' where the encapsulated 'TgtDef' is changed.
setDef :: Target -> TgtDef -> Target
setDef (Target name _) newDef = Target name newDef
setDef (Inferred name src _) newDef = Inferred name src newDef

------------------------------------------------------------------------

-- | Expanded commands of a target.
newtype Cmds = Cmds [String]

getCmds :: MkDef -> Target -> Cmds
getCmds (MkDef env _ _ _) target =
  Cmds $ fmap (expand $ Map.union internalMacros env) (getRawCmds targetDef)
  where
    targetDef :: TgtDef
    targetDef = getDef target

    internalMacros :: Env
    internalMacros =
      Map.fromList
        [ ("^", T.AssignI $ unwords (getPreqs targetDef)),
          ("@", T.AssignI $ getName target),
          ( "<",
            T.AssignI $ case target of
              Target _ _ -> ""
              Inferred _ src _ -> src
          )
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
  let def = TgtDef (exLst preqs) cmds
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
  let def = TgtDef [] cmds
   in eval' (MkDef env fstTgt ((target, def) : infs) targets) xs
eval' (MkDef env fstTgt infs targets) ((T.MkTgtRule rule) : xs) =
  let newTgtDefs = evalTgtRule env rule
      newTargets = Map.keys newTgtDefs
      initTgtDef =
        if isSpecial (head newTargets)
          then Nothing
          else Just (head newTargets)
   in case mergeDefs targets newTgtDefs of
        Nothing -> throwIO $ TargetErr MultipleDefines
        Just nt -> eval' (MkDef env (fstTgt <|> initTgtDef) infs nt) xs
  where
    -- Returns true if the target name is a special target.
    isSpecial :: String -> Bool
    isSpecial ('.' : _) = True
    isSpecial _ = False

eval :: T.MkFile -> IO MkDef
eval = eval' (MkDef Map.empty Nothing [] Map.empty)
