module Main where

import Lexer
import Parser
import Types.Core
import qualified Types.Builtins as TB
import Types.Program
import Interpretter.Core
import qualified Interpretter.Builtins as IB
import Interpretter.Program

import Control.Monad.Except

runCode :: String -> IO ()
runCode code = do
  case runExcept (lexer code >>= parser) of
    Left err -> putStrLn err >> return ()
    Right ast ->
      case runExcept $ runTCM (TB.withBuiltins $ checkProgramTypes ast) of
        Left err -> putStrLn err >> return ()
        Right _ -> do
          let ((v, o), s) = runIPM return (IB.withBuiltins $ runProgram ast)
          putStr o
          case v of
            Left err -> putStrLn err >> return ()
            Right _ -> return ()

main :: IO ()
main = do
  code <- getContents
  runCode code
