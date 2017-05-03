module Main where

import Lexer
import Parser
import Types.Core
import Types.Builtins
import Types.Program

process s = lexer s >>= parser

main :: IO ()
main = do
  code <- getContents
  case lexer code >>= parser >>= (\program -> runTCM (withBuiltins $ checkProgramTypes program)) of
    Right _ -> putStrLn "Ok"
    Left err -> putStrLn err
