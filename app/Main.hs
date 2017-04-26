module Main where

import Lexer
import Parser

process s = lexer s >>= parser

main :: IO ()
main = do
  code <- getContents
  case lexer code >>= parser of
    Right ast -> putStrLn $ show ast
    Left err -> putStrLn err
