-- | Provides an abstraction expansion of $Makefile$ macros.
module Mach.Types where

import qualified Data.Map as Map

-- | A macro assignment, can either be evaluated when the macro
-- is assigned (immediate) or when the macro is used (delayed).
data MacroAssign
  = -- | Immediate assignment
    AssignI String
  | -- | Delayed assignment
    AssignD Token
  deriving (Show)

-- | Makefile environment consisting of macro definitions.
type Env = Map.Map String MacroAssign

-- | Tokens of text which are potentially subject to macro expansion.
data Token
  = -- | Literal, not subject to macro expansion
    Lit String
  | -- | Macro expansion
    Exp Token
  | -- | Macro expansion with suffix replacement
    ExpSub Token String String
  | -- | Sequence text
    Seq [Token]
  deriving (Show, Eq)

-- | A macro definition, i.e. an assignment.
data Assign
  = Assign
      -- | Unique identifier for the macro
      String
      -- | Assignment type
      Flavor
      -- | Right value of the assignment
      Token
  deriving (Show, Eq)

-- | POSIX make supports different macro assignment operators (macro flavors).
data Flavor
  = -- | Assign and expand macro on use
    Delayed
  | -- | Assign and expand macro in definition line
    Immediate
  | -- | Immediately expand rvalue, but expand defined macro on use
    StrictDelay
  | -- | Shell command assignment, expanded on use
    System
  | -- | Conditional assignment, expanded on use
    Cond
  | -- | Appends text to a macro
    Append
  deriving (Eq)

instance Show Flavor where
  show Delayed = "="
  show Immediate = ":="
  show StrictDelay = ":::="
  show System = "!="
  show Cond = "?="
  show Append = "+="

------------------------------------------------------------------------

-- | Makefile specification, a sequence of statements.
type MkFile = [MkStat]

-- | Inference rule.
data InfRule
  = InfRule
      -- | Target, always begins with a period
      String
      -- | Commands (might be empty)
      [Token]
  deriving
    (Show, Eq)

-- | Target rule which relates targets to commands for their creation.
data TgtRule
  = TgtRule
      -- | Targets (non-empty)
      [Token]
      -- | Prerequisites
      [Token]
      -- | Commands
      [Token]
  deriving
    (Show, Eq)

-- | A statement within a @Makefile@. Three types of statements are
-- supported: assignments, includes, and rules.
data MkStat
  = MkAssign Assign
  | MkInclude [Token]
  | MkTgtRule TgtRule
  | MkInfRule InfRule
  deriving (Show, Eq)
