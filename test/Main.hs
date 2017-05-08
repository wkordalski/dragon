import Test.HUnit

--import qualified TestLexer (tests)
--import qualified TestParser (tests)
--import qualified TestSyntax (tests)
import qualified TestRunProgram (tests)

tests = TestList [
  --"Lexer"   ~: TestLexer.tests,
  --"Parser"  ~: TestParser.tests,
  --"Syntax"  ~: TestSyntax.tests,
  "Interpretter" ~: TestRunProgram.tests
  ]


main :: IO ()
main = do
  counts <- runTestTT tests
  return ()
