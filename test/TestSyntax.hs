module TestSyntax (tests) where

import Test.HUnit

import Lexer (lexer)
import Parser (parser)
import Ast

infix 4 ~~>
(~~>) :: String -> Either String Program -> Test
(~~>) s v = parse s ~?= v where
  parse :: String -> Either String Program
  parse s = lexer s >>= parser


tests = TestList [
  "Two Plus Two"                ~: testTwoPlusTwo,
  "Function declaration"        ~: testFunction,
  "Variable declaration"        ~: testVariableDecl,
  "Variable expression"         ~: testVariableExpr,
  "If"                          ~: testIf,
  "If-ElseIf-Else"              ~: testIfEiElse
  ]


testTwoPlusTwo =
  "var x :: Int = 2+2"
  ~~> Right [DVariable "x" (TNamed "Int") (EOpPlus (EInteger 2) (EInteger 2))]

testFunction =
  unlines [
  "def f a",
  "  :: Int",
  "  3+4"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "a"] [SExpr (EOpPlus (EInteger 3) (EInteger 4))]]

testVariableExpr =
  unlines [
  "def f a",
  "  :: Int",
  "  return a"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "a"] [SReturn $ EVariable "a"]]

testVariableDecl =
  unlines [
  "var x :: Int = 4"
  ]
  ~~> Right [DVariable "x" (TNamed "Int") (EInteger 4)]

testIfEiElse =
  unlines [
  "def f x",
  "  :: Bool",
  "  if x",
  "    return 1",
  "  ei x+1",
  "    return 2",
  "  else",
  "    return 3"
  ]
  ~~> Right [DFunction "f" (TNamed "Bool") [PNamed "x"] [
    SIf [(EVariable "x", [SReturn $ EInteger 1]),
         (EOpPlus (EVariable "x") (EInteger 1), [SReturn $ EInteger 2])]
        [SReturn $ EInteger 3]
  ]]

testIf =
  unlines [
  "def f x",
  "  :: Int",
  "  if x",
  "    return x",
  "  return x+x"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "x"] [
    SIf [(EVariable "x", [SReturn $ EVariable "x"])] [SPass],
    SReturn $ EOpPlus (EVariable "x") (EVariable "x")
  ]]
