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
import Control.Monad
import qualified Data.Text as T
import System.Directory

import System.FilePath ((</>))

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



tests = do
  test_files <- getRecursiveContents "testfiles/"
  let filterd_files = filter hasTestSuffix test_files
  file_tests <- mapM fileToTest filterd_files
  return $ TestList file_tests
  where
    hasTestSuffix p = T.isSuffixOf (T.pack ".test") (T.pack p)
    fileToTest p = readFile p >>= process_file p


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do                                -- 1
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)                                                  -- 2


process_file path contents =
  return $ test_name ~: (((T.unpack . T.strip . T.pack) predicted) ~?= output)
  where
    [desc, code, output] =
      map (T.unpack . T.strip) $ T.splitOn (T.pack "====\n") (T.pack contents)
    predicted = case TestRunProgram.runCode code of
      Right o -> o
      Left e -> "!!!\n" ++ e
    removeExt path = fst $ splitAt (length path - 5) path
    test_name = desc ++ " (" ++ removeExt path  ++ ")"
