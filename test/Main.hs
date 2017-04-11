import Test.HUnit

import qualified TestLexer (tests)
import qualified TestParser (tests)
import qualified TestSyntax (tests)

tests = TestList [
  "Lexer"   ~: TestLexer.tests,
  "Parser"  ~: TestParser.tests,
  "Syntax"  ~: TestSyntax.tests
  ]


main :: IO ()
main = do
  counts <- runTestTT tests
  return ()
