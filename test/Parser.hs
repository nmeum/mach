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
         in parse "foo = bar" @=? assign "foo" M.Delayed rvalue,
      testCase "Assignment with multiple blanks" $
        let rvalue = M.Seq $ Seq.fromList [M.Lit "bar"]
         in parse "foo   =     bar" @=? assign "foo" M.Delayed rvalue,
      testCase "Assignment with tab character" $
        assertBool "can only use blanks" $
          parseErr "foo \t= \tbar",
      testCase "Assignment without blanks" $
        assertBool "missing blanks" $
          parseErr "foo=bar",
      testCase "Simple macro expansion" $
        let rvalue = M.Seq $ Seq.fromList [M.Exp $ M.Lit "BAR"]
         in parse "m_exp = ${BAR}" @=? assign "m_exp" M.Delayed rvalue,
      testCase "Nested macro expansion" $
        let rvalue = M.Seq $ Seq.fromList [M.Exp $ M.Exp (M.Lit "FOO_BAR")]
         in parse "nested := ${${FOO_BAR}}" @=? assign "nested" M.Immediate rvalue
    ]
  where
    parse :: String -> Either Parsec.ParseError M.Assign
    parse = Parsec.parse P.assign ""

    parseErr :: String -> Bool
    parseErr = isLeft . parse

    assign :: T.Text -> M.Flavor -> M.Token -> Either Parsec.ParseError M.Assign
    assign n f t = Right $ M.Assign n f t

mkParser :: TestTree
mkParser = testGroup "Tests for the Makefile parser" [assignTests]
