module TestRunProgram (tests) where

import Test.HUnit

import Lexer
import Parser
import Types.Core
import qualified Types.Builtins as TB
import Types.Program
import Interpretter.Core
import qualified Interpretter.Builtins as IB
import Interpretter.Program

import Control.Monad.Except
import Control.Monad.Identity

runCode :: String -> Either String String
runCode code = do
  case runExcept (lexer code >>= parser) of
    Left err -> Left err
    Right ast ->
      case runExcept $ runTCM (TB.withBuiltins $ checkProgramTypes ast) of
        Left err -> Left err
        Right _ -> do
          (v, s) <- runIPM return (IB.mockBuiltins $ runProgram ast)
          case v of
            Left err -> Left err
            Right _ -> Right (output s)


infix 4 ~~>
(~~>) :: String -> Either String String -> Test
(~~>) s v = runCode s ~?= v

tests = TestList [
  "Empty main"                ~: testEmptyMain,
  "Print 5"                   ~: testPrintFive
  ]

testEmptyMain =
  ("def main\n" ++
  "  :: ()\n" ++
  "  return ()\n")
  ~~> Right ""


testPrintFive =
  ("def main\n" ++
  "  :: ()\n" ++
  "  return print 5\n")
  ~~> Right "5\n"
