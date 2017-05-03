module Main where

import Lexer
import Parser
import Types.Core
import Types.Builtins
import Types.Program

import Control.Monad.Except


main :: IO ()
main = do
  code <- getContents
  case runExcept (lexer code >>= parser >>= (\program -> runTCM (withBuiltins $ checkProgramTypes program))) of
    Right _ -> putStrLn "Ok"
    Left err -> putStrLn err
