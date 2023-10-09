-- | Parser combinators for the POSIX @Makefile@ specifications.
module Mach.Parser where

import Control.Monad (void)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Mach.Macro as M
import Text.ParserCombinators.Parsec (Parser, alphaNum, between, char, many, many1, noneOf, oneOf, string, (<|>))

-- | Makefile specification, a sequence of statements.
type MkFile = [MkStat]

-- | A statement within a @Makefile@. Three types of statements are
-- supported: assignments, includes, and rules.
data MkStat
  = MkAssign M.Assign
  | MkInclude T.Text
  | MkRule
      -- | Targets
      [T.Text]
      -- | Prerequisites
      [M.Token]
      -- | Commands
      [M.Token]

------------------------------------------------------------------------

-- Bind a given character to the given result.
bind :: String -> a -> Parser a
bind str val = val <$ string str

-- | Parse one or more <blank> characters.
blanks :: Parser ()
blanks = void $ many1 (char ' ')

-- | Parse a character from the portable filename character set.
--
-- See https://pubs.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap03.html#tag_03_276
fnChar :: Parser Char
fnChar = alphaNum <|> oneOf "._-"

-- | Parse a macro name, according to portable macro names should
-- consist exclusively of characters from the portabel filename
-- character set.
macroName :: Parser T.Text
macroName = T.pack <$> many1 fnChar

-- | Parse an assignment operator, also refered to as a macro flavor
-- in the POSIX standard. The implementation provided here should
-- be aligned with the 'Show' instance of 'M.Flavor'.
assignOp :: Parser M.Flavor
assignOp =
  bind "=" M.Delayed
    <|> bind ":=" M.Immediate
    <|> bind ":::=" M.StrictDelay
    <|> bind "!=" M.System
    <|> bind "?=" M.Cond
    <|> bind "+=" M.Append

-- | Parse an assignment, i.e. a macro definition.
assign :: Parser M.Assign
assign = do
  mident <- macroName
  flavor <- blanks >> assignOp <* blanks
  M.Assign mident flavor <$> tokens

-- | Parse a macro expanison.
macroExpand :: Parser M.Token
macroExpand = M.Exp <$> macroExpand'
  where
    macroExpand' :: Parser M.Token
    macroExpand' =
      char '$'
        >> ( between (char '(') (char ')') inner
               <|> between (char '{') (char '}') inner
           )

    -- Within a macro expansion, we only look for nested macro
    -- expansions, e.g. `${${BAR}}` or macro names. Using 'token' here
    -- is challenging as the closing brackets are also valid literals,
    -- hence would consume the closing bracket as a literal.
    inner :: Parser M.Token
    inner = macroExpand <|> (M.Lit <$> macroName)

-- Parse a single token, i.e. an escaped newline, escaped @$@ character, macro expansion, or literal.
token :: Parser M.Token
token =
  -- TODO: Skip comments in lexer
  macroExpand
    <|> escNewline
    <|> escDollar
    <|> litToken
  where
    escDollar :: Parser M.Token
    escDollar = bind "$$" (M.Lit $ T.pack "$")

    escNewline :: Parser M.Token
    escNewline = bind "\\\n" (M.Lit $ T.pack "\n")

    litToken :: Parser M.Token
    litToken = M.Lit <$> (T.pack <$> many1 (noneOf "#"))

-- | Parse a sequence of zero or more 'M.Token'.
tokens :: Parser M.Token
tokens = M.Seq . Seq.fromList <$> many token

-- | Parse a POSIX @Makefile@.
mkFile :: Parser MkFile
mkFile = error "not implemented"
