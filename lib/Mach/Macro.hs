-- | Provides an abstraction expansion of $Makefile$ macros.
module Mach.Macro where

import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Text as T

-- | Makefile environment consisting of macro definitions.
type Env = Map.Map T.Text T.Text

-- | Tokens of text which are potentially subject to macro expansion.
data Token
  = -- | Literal, not subject to macro expansion
    Lit T.Text
  | -- | Macro expansion
    Exp Token
  | -- | Sequence text
    Seq (Seq Token)
  deriving (Show, Eq)

-- | A macro definition, i.e. an assignment.
data Assign
  = Assign
      -- | Unique identifier for the macro
      T.Text
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

-- Expand a given macro in the context of a given environment.
expand :: Env -> Token -> T.Text
expand _ (Lit t) = t
expand env (Exp t) = case Map.lookup (expand env t) env of
  Just x -> x
  Nothing -> T.empty
expand env (Seq s) = foldr (\x acc -> expand env x `T.append` acc) T.empty s
