module Mach.Error where

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

data TargetError
  = ZeroTargetsDefined
  | MultipleDefines
  | NoTargetOrFile FilePath
  | NoSuchTarget String

instance Show TargetError where
  show ZeroTargetsDefined = "no targets defined"
  show MultipleDefines = "only one rule for a target can contain commands"
  show (NoSuchTarget tgt) = "no target named " ++ tgt ++ " was defined"
  show (NoTargetOrFile n) = "no target or file named " ++ n

data MakeErr
  = ParserErr P.ParseError
  | TargetErr TargetError
  deriving (Show)

instance Exception MakeErr
