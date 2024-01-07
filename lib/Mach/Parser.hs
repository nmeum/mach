-- | Parser combinators for the POSIX @Makefile@ specifications.
module Mach.Parser where

import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Data.List (elemIndices)
import Mach.Error (MakeErr (..))
import qualified Mach.Types as T
import Mach.Util (isSpecial)
import System.IO (hGetContents, stdin)
import Text.ParserCombinators.Parsec
  ( Parser,
    alphaNum,
    anyChar,
    between,
    char,
    eof,
    lookAhead,
    many,
    many1,
    manyTill,
    newline,
    noneOf,
    oneOf,
    optionMaybe,
    parse,
    parseFromFile,
    sepBy,
    sepBy1,
    skipMany,
    string,
    try,
    unexpected,
    (<|>),
  )

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
    <|> bind "::=" T.Immediate
    <|> bind ":::=" T.StrictDelay
    <|> bind "!=" T.System
    <|> bind "?=" T.Cond
    <|> bind "+=" T.Append

-- | Parse an assignment, i.e. a macro definition.
assign :: Parser T.Assign
assign = do
  mident <- tokens' [' ', '=']
  flavor <- blanks >> assignOp <* blanks
  T.Assign mident flavor <$> tokens

-- | Parse a macro expansion.
macroExpand :: Parser T.Token
macroExpand = char '$' >> (singleChar <|> macroExpand')
  where
    singleChar :: Parser T.Token
    singleChar = T.Exp <$> ((\x -> T.Lit [x]) <$> literal "{}()")

-- | Parse a macro expansion enclosed in parentheses.
macroExpand' :: Parser T.Token
macroExpand' =
  let inner = try subExpand <|> simpleExpand
   in ( between (char '(') (char ')') inner
          <|> between (char '{') (char '}') inner
      )
  where
    -- Parse a macro expansion of the form $(string1).
    simpleExpand :: Parser T.Token
    simpleExpand =
      T.Exp
        <$> ( macroExpand
                <|> macroExpand
                <|> tokenLit (literal "})")
            )

    -- Parse a macro expansion of the form $(string1:subst1=[subst2]).
    subExpand :: Parser T.Token
    subExpand = do
      -- TODO: Support nested macro expansion in string1
      string1 <- T.Exp <$> (tokenLit $ literal ":})")
      _ <- char ':'
      subst1 <- many1 $ noneOf "="
      _ <- char '='
      subst2 <- many $ noneOf "})"
      pure $ T.ExpSub string1 subst1 subst2

-- | Parse a valid character for a literal.
-- Characters not valid in the current context can be passed as well.
literal :: [Char] -> Parser Char
literal notValid = noneOf $ notValid ++ "#\n\\$"

-- | Parse a single token, i.e. an escaped newline, escaped @$@ character, macro expansion, or literal.
token :: Parser T.Token
token = tokenLit $ literal []

-- | Parse a token but use a custom parser for parsing of literal tokens.
tokenLit :: Parser Char -> Parser T.Token
tokenLit lit =
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
    litToken = T.Lit <$> many1 lit

-- | Parse a sequence of zero or more 'T.Token'.
tokens :: Parser T.Token
tokens = T.Seq <$> many token

-- | Like 'tokens' but allows passing characters not valid in the current context.
tokens' :: [Char] -> Parser T.Token
tokens' notValid = T.Seq <$> many (tokenLit $ literal notValid)

-- | Parse multiple lines of commands prefixed by a tab character.
commands :: Parser [T.Token]
commands = many (char '\t' >> (tokens <* newline))

-- | Inference rule.
infRule :: Parser T.InfRule
infRule = do
  target <- char '.' >> (:) '.' <$> many1 targetChar

  let periods = length $ elemIndices '.' target
  when (isSpecial target || (periods /= 1 && periods /= 2)) $
    unexpected "invalid inference rule target name"

  cmds <-
    char ':'
      >> ( (const [] <$> (many1 blank >> char ';'))
             <|> (many blank >> newline >> commands)
         )

  pure $ T.InfRule target cmds

-- | Target rule which defines how targets are build.
targetRule :: Parser T.TgtRule
targetRule = do
  targets <- sepBy1 (tokenLit targetChar) blank
  _ <- char ':' >> (blank <|> lookAhead newline)
  prereqs <- sepBy (tokenLit targetChar) blank
  command <- optionMaybe (char ';' >> tokens)
  _ <- newline

  cmds <- commands
  pure $ T.TgtRule targets prereqs (maybe cmds (: cmds) command)

include :: Parser [T.Token]
include = do
  _ <- optionMaybe (char '-')
  _ <- string "include" >> blanks
  paths <- sepBy1 (tokenLit $ literal " ") blank
  _ <- maybeBlanks
  pure paths

skipNoCode :: Parser ()
skipNoCode = do
  _ <- many blank
  _ <- many newline
  _ <- skipMany (try comment >> many blank >> many newline)
  pure ()
  where
    comment :: Parser String
    comment = char '#' >> manyTill anyChar newline

lexeme :: Parser a -> Parser a
lexeme p = p <* skipNoCode

-- | Parse a POSIX @Makefile@.
mkFile :: Parser T.MkFile
mkFile =
  skipNoCode
    >> many
      ( try (T.MkAssign <$> lexeme assign)
          <|> try (T.MkInfRule <$> lexeme infRule)
          <|> try (T.MkTgtRule <$> lexeme targetRule)
          <|> try (T.MkInclude <$> lexeme include)
      )
      -- Ensure that we parse the whole Makefile
      <* lexeme eof

------------------------------------------------------------------------

-- | Parse assignments and targets specified on the command-line.
cmdLine :: String -> IO (T.MkFile, [FilePath])
cmdLine str =
  case parse cmdLine' "command-line" str of
    Left err -> throwIO $ ParserErr err
    Right mk -> pure mk
  where
    assign' :: Parser T.Assign
    assign' = do
      mident <- tokens' [' ', '=']
      flavor <- assignOp
      unless (flavor `elem` [T.Delayed, T.Immediate, T.StrictDelay]) $
        unexpected "invalid assignment flavor"

      T.Assign mident flavor <$> tokens' " "

    cmdLine' :: Parser (T.MkFile, [FilePath])
    cmdLine' = do
      assigns <- many (try assign' <* many blank)
      targets <- sepBy (many1 targetChar) blank
      pure (map T.MkAssign assigns, targets)

parseMkFile :: FilePath -> IO T.MkFile
parseMkFile path = do
  res <-
    if path == "-"
      then parse mkFile path <$> hGetContents stdin
      else parseFromFile mkFile path
  case res of
    Left err -> throwIO $ ParserErr err
    Right mk -> pure mk
