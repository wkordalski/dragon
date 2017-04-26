module TestSyntax (tests) where

import Test.HUnit

import Lexer (lexer)
import Parser (parser)
import Ast

infix 4 ~~>
(~~>) :: String -> Either String Program -> Test
(~~>) s v = parse s ~?= v where
  parse :: String -> Either String Program
  parse s = lexer s >>= parser


tests = TestList [
  "Two Plus Two"                ~: testTwoPlusTwo,
  "Function declaration"        ~: testFunction,
  "Operators"                   ~: testOperators,
  "Multiple statements"         ~: testMultiStatements,
  "Variable declaration"        ~: testVariableDecl,
  "Variable expression"         ~: testVariableExpr,
  "Multiple definitions"        ~: testMultiDefinitions,
  "If"                          ~: testIf,
  "If-ElseIf-Else"              ~: testIfEiElse
  ]


testTwoPlusTwo =
  "var x :: Int = 2+2"
  ~~> Right [DVariable "x" (TNamed "Int") (EOpAdd (EInteger 2) (EInteger 2))]

testOperators =
  unlines [
  "def f a",
  "  :: Int",
  "  a = \\x (:: Bool) -> (x.field a + + 3** - 4** 5 * 2 + 2 * 3, not true and (false or true), 2 < 4)",
  "  b += 3 + 4 // 2",
  "  return a == b"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "a"] [
    SExpr (EOpAssign (EVariable "a") (ELambda (TNamed "Bool") [PNamed "x"] (ETuple [
      EOpAdd (EOpAdd (ECall (EMember (EVariable "x") "field") (EVariable "a")) (EOpMultiply (EUOpPlus (EOpPower (EInteger 3) (EUOpMinus (EOpPower (EInteger 4) (EInteger 5))))) (EInteger 2))) (EOpMultiply (EInteger 2) (EInteger 3)),
      EOpConjunction (EOpNegation (EBoolean True)) (EOpAlternative (EBoolean False) (EBoolean True)),
      EOpLessThan (EInteger 2) (EInteger 4)
    ]))),
    SExpr (EOpAssignAdd (EVariable "b") (EOpAdd (EInteger 3) (EOpIntDivision (EInteger 4) (EInteger 2)))),
    SReturn (EOpEqual (EVariable "a") (EVariable "b"))
  ]]

testFunction =
  unlines [
  "def f a",
  "  :: Int -> Int",
  "  3+4"
  ]
  ~~> Right [DFunction "f" (TFunction (TNamed "Int") (TNamed "Int")) [PNamed "a"] [SExpr (EOpAdd (EInteger 3) (EInteger 4))]]

testVariableExpr =
  unlines [
  "def f a",
  "  :: Int",
  "  return a"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "a"] [SReturn $ EVariable "a"]]

testVariableDecl =
  unlines [
  "var x :: Int = 4"
  ]
  ~~> Right [DVariable "x" (TNamed "Int") (EInteger 4)]

testIfEiElse =
  unlines [
  "def f x",
  "  :: Bool",
  "  if x",
  "    return 1",
  "  ei x+1",
  "    return 2",
  "  else",
  "    return 3"
  ]
  ~~> Right [DFunction "f" (TNamed "Bool") [PNamed "x"] [
    SIf [(EVariable "x", [SReturn $ EInteger 1]),
         (EOpAdd (EVariable "x") (EInteger 1), [SReturn $ EInteger 2])]
        [SReturn $ EInteger 3]
  ]]

testIf =
  unlines [
  "def f x",
  "  :: Int",
  "  if x",
  "    return x",
  "  return x+x"
  ]
  ~~> Right [DFunction "f" (TNamed "Int") [PNamed "x"] [
    SIf [(EVariable "x", [SReturn $ EVariable "x"])] [SPass],
    SReturn $ EOpAdd (EVariable "x") (EVariable "x")
  ]]

testMultiDefinitions =
  unlines [
  "var a :: Int = 3",
  "",
  "def f x",
  "  :: Int -> Int",
  "  return x + a",
  "def g x y",
  "  :: Int -> Int -> Int -> Int",
  "  return \\z (:: Int -> Int) -> a * x + y * z"
  ]
  ~~>
    let ti = TNamed "Int" in
    Right [
    DVariable "a" ti (EInteger 3),
    DFunction "f" (TFunction ti ti) [PNamed "x"] [SReturn $ EOpAdd (EVariable "x") (EVariable "a")],
    DFunction "g" (TFunction ti $ TFunction ti $ TFunction ti ti) [PNamed "x", PNamed "y"] [
      SReturn $ ELambda (TFunction ti ti) [PNamed "z"] (EOpAdd (EOpMultiply (EVariable "a") (EVariable "x")) (EOpMultiply (EVariable "y") (EVariable "z")))
      ]
    ]

testMultiStatements =
  unlines [
  "def silnia n",
  "  :: Int -> Int",
  "  ",
  "  if n <= 1",
  "    return 1",
  "  ",
  "  return n * silnia (n-1)",
  "    "
  ]
  ~~>
    Right [ DFunction "silnia" (TFunction (TNamed "Int") (TNamed "Int")) [PNamed "n"] [
      SIf [(EOpLessEqualThan (EVariable "n") (EInteger 1), [
        SReturn $ EInteger 1
      ])] [ SPass ],
      SReturn $ EOpMultiply (EVariable "n") (ECall (EVariable "silnia") (EOpSubtract (EVariable "n") (EInteger 1)))
    ]]
