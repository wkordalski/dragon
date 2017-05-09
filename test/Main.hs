import Test.HUnit

import qualified TestRunProgram (tests)

tests = [
  ]

main :: IO ()
main = do
  interp_tests <- TestRunProgram.tests
  counts <- runTestTT $ TestList (tests ++ ["Interpretter" ~: interp_tests])
  return ()
