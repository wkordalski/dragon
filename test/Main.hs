import Test.HUnit

import qualified TestLexer (tests)
import qualified TestParser (tests)

tests = TestList [
  "Lexer"   ~: TestLexer.tests,
  "Parser"  ~: TestParser.tests
  ]


main :: IO ()
main = do
  counts <- runTestTT tests
  return ()
