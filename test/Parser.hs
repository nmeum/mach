{-# LANGUAGE OverloadedStrings #-}

module Parser (mkParser) where

import Data.Either (isLeft)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Mach.Macro as M
import qualified Mach.Parser as P
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec

assignTests :: TestTree
assignTests =
  testGroup
    "assignments"
    [ testCase "Simple assignment of macro to string" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "bar"]
         in parse "foo = bar" @?= assign "foo" M.Delayed rvalue,
      testCase "Assignment with multiple blanks" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "bar"]
         in parse "foo   =     bar" @?= assign "foo" M.Delayed rvalue,
      testCase "Assignment with tab character" $
        assertBool "can only use blanks" $
          parseErr "foo \t= \tbar",
      testCase "Assignment without blanks" $
        assertBool "missing blanks" $
          parseErr "foo=bar",
      testCase "Simple macro expansion" $
        let rvalue = M.Seq $ Seq.fromList [M.Exp $ M.Lit "BAR"]
         in parse "m_exp = ${BAR}" @?= assign "m_exp" M.Delayed rvalue,
      testCase "Nested macro expansion" $
        let rvalue = M.Seq $ Seq.fromList [M.Exp $ M.Exp (M.Lit "FOO_BAR")]
         in parse "nested := ${${FOO_BAR}}" @?= assign "nested" M.Immediate rvalue,
      testCase "Multi-token assignment" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "a", M.Exp (M.Lit "b"), M.Lit "c"]
         in parse "_ ?= a${b}c" @?= assign "_" M.Cond rvalue,
      testCase "Assignment with escaped dollar" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "$", M.Lit "foo", M.Lit "$", M.Lit "bar"]
         in parse "a = $$foo$$bar" @?= assign "a" M.Delayed rvalue,
      testCase "Assignment with escaped newline" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "foo", M.Lit " ", M.Lit "bar"]
         in parse "a = foo\\\nbar" @?= assign "a" M.Delayed rvalue,
      testCase "Invalid macro expansion" $
        assertBool "closing brackets are not valid macro names" $
          parseErr "foo = ${}}"
    ]
  where
    parse :: String -> Either Parsec.ParseError M.Assign
    parse = Parsec.parse P.assign ""

    parseErr :: String -> Bool
    parseErr = isLeft . parse

    assign :: T.Text -> M.Flavor -> M.Token -> Either Parsec.ParseError M.Assign
    assign n f t = Right $ M.Assign n f t

ruleTests :: TestTree
ruleTests =
  testGroup
    "target rules"
    [ testCase "Single target, single prerequisite, single command" $
        parse "foo: bar\n\tbaz\n" @?= rule [M.Lit "foo"] [M.Lit "bar"] [M.Seq $ Seq.fromList [M.Lit "baz"]],
      testCase "No prerequisite" $
        parse "foo:\n\tbar\n" @?= rule [M.Lit "foo"] [] [M.Seq $ Seq.fromList [M.Lit "bar"]],
      testCase "No prerequisite, no commands" $
        parse "foo:\n" @?= rule [M.Lit "foo"] [] [],
      testCase "Multiple targets" $
        parse "foo bar baz:\n\tcommand\n" @?= rule [M.Lit "foo", M.Lit "bar", M.Lit "baz"] [] [M.Seq $ Seq.fromList [M.Lit "command"]],
      testCase "Command on same line" $
        parse "foo: bar baz;echo foo\n\techo bar\n" @?= rule [M.Lit "foo"] [M.Lit "bar", M.Lit "baz"] [M.Seq $ Seq.fromList [M.Lit "echo foo"], M.Seq $ Seq.fromList [M.Lit "echo bar"]],
      testCase "Target with macro expansion" $
        parse "$(MAIN):\n" @?= rule [M.Exp (M.Lit "MAIN")] [] [],
      testCase "Command with macro expansion" $
        parse "foo:\n\techo ${BAR}\n" @?= rule [M.Lit "foo"] [] [M.Seq (Seq.fromList [M.Lit "echo ", M.Exp (M.Lit "BAR")])]
    ]
  where
    parse :: String -> Either Parsec.ParseError P.Rule
    parse = Parsec.parse P.targetRule ""

    rule :: [M.Token] -> [M.Token] -> [M.Token] -> Either Parsec.ParseError P.Rule
    rule t p c = Right $ P.Rule (Seq.fromList t) (Seq.fromList p) (Seq.fromList c)

includeTests :: TestTree
includeTests =
  testGroup
    "include lines"
    [ testCase "single file path" $
        parse "include foo" @?= mkPaths [M.Lit "foo"],
      testCase "multiple paths" $
        parse "include foo bar baz" @?= mkPaths [M.Lit "foo", M.Lit "bar", M.Lit "baz"],
      testCase "path with macro expansion" $
        parse "include ${FILE}" @?= mkPaths [M.Exp (M.Lit "FILE")],
      testCase "include prefixed with minus" $
        parse "-include foo" @?= mkPaths [M.Lit "foo"]
    ]
  where
    parse :: String -> Either Parsec.ParseError (Seq.Seq M.Token)
    parse = Parsec.parse P.include ""

    mkPaths :: [M.Token] -> Either Parsec.ParseError (Seq.Seq M.Token)
    mkPaths = Right . Seq.fromList

mkTests :: TestTree
mkTests =
  testGroup
    "makefile parser"
    [ testCase "assignment" $
        let assign = M.Assign "foo" M.Delayed (M.Seq $ Seq.fromList [M.Lit "bar"])
         in parse "foo = bar\n" @?= Right [P.MkAssign assign],
      testCase "rule" $
        let rule = P.Rule (Seq.fromList [M.Lit "foo"]) (Seq.empty) (Seq.fromList [M.Seq $ Seq.fromList [M.Lit "bar"]])
         in parse "foo:\n\tbar\n" @?= Right [P.MkRule rule],
      testCase "include" $
        let paths = Seq.fromList [M.Lit "foo", M.Lit "bar"]
         in parse "include foo bar\n" @?= Right [P.MkInclude paths]
    ]
  where
    parse :: String -> Either Parsec.ParseError [P.MkStat]
    parse = Parsec.parse P.mkFile ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the Makefile parser"
    [assignTests, ruleTests, includeTests, mkTests]
