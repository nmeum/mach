module Parser (mkParser) where

import Data.Either (isLeft)
import qualified Mach.Parser as P
import qualified Mach.Types as T
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec

assignTests :: TestTree
assignTests =
  testGroup
    "assignments"
    [ testCase "Simple assignment of macro to string" $
        let rvalue = T.Seq [T.Lit "bar"]
         in parse "foo = bar" @?= assign "foo" T.Delayed rvalue,
      testCase "Assignment with multiple blanks" $
        let rvalue = T.Seq [T.Lit "bar"]
         in parse "foo   =     bar" @?= assign "foo" T.Delayed rvalue,
      testCase "Assignment with tab character" $
        assertBool "can only use blanks" $
          parseErr "foo \t= \tbar",
      testCase "Assignment without blanks" $
        assertBool "missing blanks" $
          parseErr "foo=bar",
      testCase "Simple macro expansion" $
        let rvalue = T.Seq [T.Exp $ T.Lit "BAR"]
         in parse "m_exp = ${BAR}" @?= assign "m_exp" T.Delayed rvalue,
      testCase "Nested macro expansion" $
        let rvalue = T.Seq [T.Exp $ T.Exp (T.Lit "FOO_BAR")]
         in parse "nested := ${${FOO_BAR}}" @?= assign "nested" T.Immediate rvalue,
      testCase "Multi-token assignment" $
        let rvalue = T.Seq [T.Lit "a", T.Exp (T.Lit "b"), T.Lit "c"]
         in parse "_ ?= a${b}c" @?= assign "_" T.Cond rvalue,
      testCase "Assignment with escaped dollar" $
        let rvalue = T.Seq [T.Lit "$", T.Lit "foo", T.Lit "$", T.Lit "bar"]
         in parse "a = $$foo$$bar" @?= assign "a" T.Delayed rvalue,
      testCase "Assignment with escaped newline" $
        let rvalue = T.Seq [T.Lit "foo", T.Lit " ", T.Lit "bar"]
         in parse "a = foo\\\nbar" @?= assign "a" T.Delayed rvalue,
      testCase "Invalid macro expansion" $
        assertBool "closing brackets are not valid macro names" $
          parseErr "foo = ${}}",
      testCase "Macro expansion with substition" $
        let rvalue = T.Seq [T.ExpSub (T.Lit "string1") "subst1" "subst2"]
         in parse "a = ${string1:subst1=subst2}" @?= assign "a" T.Delayed rvalue
    ]
  where
    parse :: String -> Either Parsec.ParseError T.Assign
    parse = Parsec.parse P.assign ""

    parseErr :: String -> Bool
    parseErr = isLeft . parse

    assign :: String -> T.Flavor -> T.Token -> Either Parsec.ParseError T.Assign
    assign n f t = Right $ T.Assign n f t

ruleTests :: TestTree
ruleTests =
  testGroup
    "target rules"
    [ testCase "Single target, single prerequisite, single command" $
        parse "foo: bar\n\tbaz\n" @?= rule [T.Lit "foo"] [T.Lit "bar"] [T.Seq [T.Lit "baz"]],
      testCase "No prerequisite" $
        parse "foo:\n\tbar\n" @?= rule [T.Lit "foo"] [] [T.Seq [T.Lit "bar"]],
      testCase "No prerequisite, no commands" $
        parse "foo:\n" @?= rule [T.Lit "foo"] [] [],
      testCase "Multiple targets" $
        parse "foo bar baz:\n\tcommand\n" @?= rule [T.Lit "foo", T.Lit "bar", T.Lit "baz"] [] [T.Seq [T.Lit "command"]],
      testCase "Command on same line" $
        parse "foo: bar baz;echo foo\n\techo bar\n" @?= rule [T.Lit "foo"] [T.Lit "bar", T.Lit "baz"] [T.Seq [T.Lit "echo foo"], T.Seq [T.Lit "echo bar"]],
      testCase "Target with macro expansion" $
        parse "$(MAIN):\n" @?= rule [T.Exp (T.Lit "MAIN")] [] [],
      testCase "Command with macro expansion" $
        parse "foo:\n\techo ${BAR}\n" @?= rule [T.Lit "foo"] [] [T.Seq [T.Lit "echo ", T.Exp (T.Lit "BAR")]]
    ]
  where
    parse :: String -> Either Parsec.ParseError T.Rule
    parse = Parsec.parse P.targetRule ""

    rule :: [T.Token] -> [T.Token] -> [T.Token] -> Either Parsec.ParseError T.Rule
    rule t p c = Right $ T.Rule t p c

includeTests :: TestTree
includeTests =
  testGroup
    "include lines"
    [ testCase "single file path" $
        parse "include foo" @?= mkPaths [T.Lit "foo"],
      testCase "multiple paths" $
        parse "include foo bar baz" @?= mkPaths [T.Lit "foo", T.Lit "bar", T.Lit "baz"],
      testCase "path with macro expansion" $
        parse "include ${FILE}" @?= mkPaths [T.Exp (T.Lit "FILE")],
      testCase "include prefixed with minus" $
        parse "-include foo" @?= mkPaths [T.Lit "foo"]
    ]
  where
    parse :: String -> Either Parsec.ParseError [T.Token]
    parse = Parsec.parse P.include ""

    mkPaths :: [T.Token] -> Either Parsec.ParseError [T.Token]
    mkPaths = Right

mkTests :: TestTree
mkTests =
  testGroup
    "makefile parser"
    [ testCase "assignment" $
        let assign = T.Assign "foo" T.Delayed (T.Seq [T.Lit "bar"])
         in parse "foo = bar\n" @?= Right [T.MkAssign assign],
      testCase "rule" $
        let rule = T.Rule [T.Lit "foo"] [] [T.Seq [T.Lit "bar"]]
         in parse "foo:\n\tbar\n" @?= Right [T.MkRule rule],
      testCase "include" $
        let paths = [T.Lit "foo", T.Lit "bar"]
         in parse "include foo bar\n" @?= Right [T.MkInclude paths]
    ]
  where
    parse :: String -> Either Parsec.ParseError [T.MkStat]
    parse = Parsec.parse P.mkFile ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the Makefile parser"
    [assignTests, ruleTests, includeTests, mkTests]
