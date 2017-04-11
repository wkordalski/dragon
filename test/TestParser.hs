module TestParser (tests) where

import Test.HUnit

import Lexer
import Parser
import Ast

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
  -- "Identifier token"          ~: testIdent,
  -- "Number token"              ~: testNumber,
  -- "Add operator"              ~: testAddOperator,
  -- "Simple numeric expression" ~: testSimpleNumExpr,
  -- "Comment removeal"          ~: testComment,
  -- "Single indent"             ~: testSingleIndent
  ]


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
