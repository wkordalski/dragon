module TestParser (tests) where

import Test.HUnit

import Lexer
import Parser

-- runLexer :: String -> Either String [Token]
-- runLexer s =
--   case lexer s of
--     Right tree -> Right $ map fst tree
--     Left err -> Left err
--
-- infix 4 ~~>
-- (~~>) :: String -> Either String [Token] -> Test
-- (~~>) s v = runLexer s ~?= v

tests = TestList [
  "Two Plus Two"                ~: testTwoPlusTwo
  -- "Identifier token"          ~: testIdent,
  -- "Number token"              ~: testNumber,
  -- "Add operator"              ~: testAddOperator,
  -- "Simple numeric expression" ~: testSimpleNumExpr,
  -- "Comment removeal"          ~: testComment,
  -- "Single indent"             ~: testSingleIndent
  ]



tnl = TNewline
eof = TEof
tint = TInteger
tid = TIdentifier
tind = TIndent
tded = TDedent

testTwoPlusTwo = parser [tint 2, TOpAdd, tint 2, tnl, tnl, eof] ~?=
  [AOpAdd (AInteger 2) (AInteger 2)]

-- testIdent = "asd" ~~> Right [tid "asd", tnl, tnl, eof]
--
-- testNumber = "9" ~~> Right [tint 9, tnl, tnl, eof]
--
-- testAddOperator = "+" ~~> Right [TOpAdd, tnl, tnl, eof]
--
-- testSimpleNumExpr = "2+6" ~~> Right [tint 2, TOpAdd, tint 6, tnl, tnl, eof]
--
-- testComment = "asd # some comment\nbdf" ~~>
--   Right [tid "asd", tnl, tid "bdf", tnl, tnl,  eof ]
--
-- testSingleIndent = "asd \n  bdf\nzxc" ~~>
--   Right [tid "asd", tnl, tind, tid "bdf", tnl, tded, tid "zxc", tnl, tnl, eof]
