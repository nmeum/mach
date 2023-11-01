module Mach.Error where

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

data TargetError = ZeroTargetsDefined | NoTargetOrFile FilePath

instance Show TargetError where
  show ZeroTargetsDefined = "no targets defined"
  show (NoTargetOrFile n) = "no target or file named " ++ n

data MakeErr
  = ParserErr P.ParseError
  | TargetErr TargetError
  deriving (Show)

instance Exception MakeErr
