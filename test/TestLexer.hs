module TestLexer (tests) where

import Test.HUnit

import Lexer

infix 4 ~~>
(~~>) :: String -> Either String [Token] -> Test
(~~>) s v = (fmap (map fst) $ lexer s) ~?= v

tests = TestList [
  "Identifier token"          ~: testIdent,
  "Number token"              ~: testNumber,
  "Add operator"              ~: testAddOperator,
  "Simple numeric expression" ~: testSimpleNumExpr,
  "Comment removeal"          ~: testComment,
  "Single indent"             ~: testSingleIndent
  ]

tnl = TNewline
eof = TEof
tint = TInteger
tid = TIdentifier
tind = TIndent
tded = TDedent
top = TOperator

testIdent = "asd" ~~> Right [tid "asd", tnl, tnl, eof]

testNumber = "9" ~~> Right [tint 9, tnl, tnl, eof]

testAddOperator = "+" ~~> Right [top "+", tnl, tnl, eof]

testSimpleNumExpr = "2+6" ~~> Right [tint 2, top "+", tint 6, tnl, tnl, eof]

testComment = "asd # some comment\nbdf" ~~>
  Right [tid "asd", tnl, tid "bdf", tnl, tnl,  eof ]

testSingleIndent = "asd \n  bdf\nzxc" ~~>
  Right [tid "asd", tnl, tind, tid "bdf", tnl, tded, tid "zxc", tnl, tnl, eof]
