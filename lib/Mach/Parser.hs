-- | Parser combinators for the POSIX @Makefile@ specifications.
module Mach.Parser where

import Control.Exception (throwIO)
import Control.Monad (void)
import Mach.Error (MakeErr (..))
import Mach.Eval (MkDef (..), eval, expand)
import qualified Mach.Types as T
import Text.ParserCombinators.Parsec (Parser, alphaNum, between, char, lookAhead, many, many1, newline, noneOf, oneOf, optionMaybe, parseFromFile, sepBy, sepBy1, string, try, (<|>))

-- Bind a given character to the given result.
bind :: String -> a -> Parser a
bind str val = val <$ string str

-- | Parse one or more newlines.
newlines :: Parser ()
newlines = void $ many1 newline

-- | Parse a single blank character.
blank :: Parser Char
blank = char ' '

-- | Parse one or more <blank> characters.
blanks :: Parser ()
blanks = void $ many1 blank

-- Parse zero or more <blank> characters.
maybeBlanks :: Parser ()
maybeBlanks = void $ many (char ' ')

-- | Parse a character from the portable filename character set.
--
-- See https://pubs.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap03.html#tag_03_276
fnChar :: Parser Char
fnChar = alphaNum <|> oneOf "._-"

-- | Parse a macro name, according to portable macro names should
-- consist exclusively of characters from the portabel filename
-- character set.
macroName :: Parser String
macroName = many1 fnChar

-- | Parse a target name character. As per POSIX, target names should
-- only consist of slashes, hyphens, periods, underscores, digits and
-- alphabetics.
targetChar :: Parser Char
targetChar = alphaNum <|> oneOf "/-._"

-- | Parse an assignment operator, also refered to as a macro flavor
-- in the POSIX standard. The implementation provided here should
-- be aligned with the 'Show' instance of 'T.Flavor'.
assignOp :: Parser T.Flavor
assignOp =
  bind "=" T.Delayed
    <|> bind ":=" T.Immediate
    <|> bind ":::=" T.StrictDelay
    <|> bind "!=" T.System
    <|> bind "?=" T.Cond
    <|> bind "+=" T.Append

-- | Parse an assignment, i.e. a macro definition.
assign :: Parser T.Assign
assign = do
  mident <- macroName
  flavor <- blanks >> assignOp <* blanks
  T.Assign mident flavor <$> tokens

-- | Parse a macro expanison.
macroExpand :: Parser T.Token
macroExpand = T.Exp <$> macroExpand'
  where
    macroExpand' :: Parser T.Token
    macroExpand' =
      char '$'
        >> ( between (char '(') (char ')') inner
               <|> between (char '{') (char '}') inner
           )

    -- Within a macro expansion, we only look for nested macro
    -- expansions, e.g. `${${BAR}}` or macro names. Using 'token' here
    -- is challenging as the closing brackets are also valid literals,
    -- hence would consume the closing bracket as a literal.
    inner :: Parser T.Token
    inner = macroExpand <|> (T.Lit <$> macroName)

-- | Parse a single token, i.e. an escaped newline, escaped @$@ character, macro expansion, or literal.
token :: Parser T.Token
token = tokenLit $ noneOf "#\n\\$"

-- | Parse a token but use a custom parser for parsing of literal tokens.
tokenLit :: Parser Char -> Parser T.Token
tokenLit literal =
  -- TODO: Skip comments in lexer
  try macroExpand
    <|> escDollar
    <|> escNewline
    <|> litToken
  where
    escDollar :: Parser T.Token
    escDollar = bind "$$" (T.Lit "$")

    escNewline :: Parser T.Token
    escNewline = T.Lit " " <$ (string "\\\n" >> maybeBlanks)

    -- TODO: In noneOf, check that \ is followed by a newline.
    litToken :: Parser T.Token
    litToken = T.Lit <$> many1 literal

-- | Parse a sequence of zero or more 'T.Token'.
tokens :: Parser T.Token
tokens = T.Seq <$> many token

-- | Target rule which defines how targets are build.
targetRule :: Parser T.Rule
targetRule = do
  targets <- sepBy1 (tokenLit targetChar) blank
  _ <- char ':' >> (blank <|> lookAhead newline)
  prereqs <- sepBy (tokenLit targetChar) blank
  command <- optionMaybe (char ';' >> tokens)
  _ <- newline

  cmds <- many (char '\t' >> (tokens <* newline))
  pure $ T.Rule targets prereqs (maybe cmds (: cmds) command)

include :: Parser [T.Token]
include = do
  _ <- optionMaybe (char '-')
  _ <- string "include" >> blanks
  paths <- sepBy1 (tokenLit $ noneOf " #\n\\$") blank
  _ <- maybeBlanks
  pure paths

-- | Parse a POSIX @Makefile@.
mkFile :: Parser T.MkFile
mkFile =
  many
    ( try (T.MkAssign <$> assign <* newlines)
        <|> try (T.MkRule <$> targetRule)
        <|> try (T.MkInclude <$> include <* newlines)
    )

------------------------------------------------------------------------

parseMkFile :: FilePath -> IO (T.MkFile)
parseMkFile path = do
  res <- parseFromFile mkFile path
  case res of
    Left err -> throwIO $ ParserErr err
    Right mk -> pure mk

makefile :: FilePath -> IO (MkDef, Maybe String)
makefile path = do
  mk <- parseMkFile path
  let mkDef = eval mk

  -- TODO: Refactor extraction of first target.
  pure (mkDef, firstRule mk >>= Just . firstTarget mkDef)
  where
    firstRule :: T.MkFile -> Maybe T.Rule
    firstRule [] = Nothing
    firstRule ((T.MkRule rule) : _) = Just rule
    firstRule (_ : xs) = firstRule xs

    firstTarget :: MkDef -> T.Rule -> String
    firstTarget (MkDef env _) (T.Rule targets _ _) =
      expand env $ head targets
