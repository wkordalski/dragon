{
module Parser where

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace

import Ast
import Lexer
}

%name parserWrapper
%monad { Parser }
%lexer { lexerWrapper } { TEof }
%tokentype { Token }
%error { parseError }

%token
  id      { TIdentifier $$ }
  int     { TInteger $$ }

  def     { TKeyword "def" }
  var     { TKeyword "var" }
  return  { TKeyword "return" }

  if      { TKeyword "if" }
  ei      { TKeyword "ei" }
  else    { TKeyword "else" }

  while   { TKeyword "while" }

  '='     { TOperator "="}
  '+'     { TOperator "+" }
  '::'    { TOperator "::" }
  ','     { TOperator "," }

  '\n'    { TNewline }
  '\>'    { TIndent }
  '\<'    { TDedent }
  eof     { TEof }

%%

Program : Declarations           { $1 }

Declarations : Declaration              { [$1] }
Declarations : Declaration Declarations { $1 : $2 }
Declarations : '\n'                     { [] }
Declarations : '\n' Declarations        { $2 }

Declaration : var id '::' TypeExpr '=' Expr '\n'                                    { DVariable $2 $4  $6 }
Declaration : def id ArgsPatternMatch '::' TypeExpr '=' Expr '\n'                   { DFunction $2 $5 $3 [SReturn $7] }
Declaration : def id ArgsPatternMatch '\n' '\>' '::' TypeExpr '\n' Statements '\<'  { DFunction $2 $7 $3 $9 }

Statements : Statement                { [$1] }
Statements : Statement Statements     { $1 : $2 }
Statements : '\n' Statements          { $2 }

Statement : Expr '\n'                             { SExpr $1 }
Statement : return Expr '\n'                      { SReturn $2 }
Statement : var id '::' TypeExpr '=' Expr '\n'    { SVariable $2 $4 $6 }
Statement : IfIf IfEiMany IfElseMaybe             { SIf ($1:$2) $3 }
Statement : while Expr '\n' '\>' Statements '\<'  { SWhile $2 $5 }
Statement : while Expr ',' Statement              { SWhile $2 [$4] }

IfIf : if Expr '\n' '\>' Statements '\<'  { ($2, $5) }
     | if Expr ',' Statement              { ($2, [$4]) }
IfEi : ei Expr '\n' '\>' Statements '\<'  { ($2, $5) }
     | ei Expr ',' Statement              { ($2, [$4]) }
IfElse : else '\n' '\>' Statements '\<'   { $4 }
       | else ',' Statement               { [$3] }
IfEiMany : IfEi IfEiMany                  { $1 : $2 }
         | {- empty -}                    { [] }
IfElseMaybe : IfElse                      { $1 }
            | {- empty -}                 { [SPass] }

Frac : int            { EInteger $1 }
     | id             { EVariable $1 }
Expr : Frac           { $1 }
     | Expr '+' Frac  { EOpPlus $1 $3 }

ArgsPatternMatch : {- empty -}                    { [] }
ArgsPatternMatch : PatternMatch ArgsPatternMatch  { $1 : $2 }

PatternMatch : id         { PNamed $1 }

-- add function type
TypeExpr : id             { TNamed $1 }

{

parseError _ = do
  ParserState {range=range} <- get
  fail $ "Parse error " ++ show range


data ParserState = ParserState { tokens :: [PlacedToken], range :: Range } deriving (Show)

type Parser ttt = StateT ParserState (Either String) ttt

betterPlace (Place (a, b, c)) (Place (d, e, f)) =
  Place ((newer a d), (newer b e), (newer c f))
  where
    newer a b = if b == -1 then a else b

betterRange (Range (p1, p2)) (Range(p3, p4)) =
  Range ((betterPlace p1 p3), (betterPlace p2 p4))

lexerWrapper :: (Token -> Parser a) -> Parser a
lexerWrapper cont = do
  ParserState {tokens=((token, range):rest), range=old_range} <- get
  modify (\s -> ParserState {tokens=rest, range=(betterRange old_range range)})
  cont token

parser :: [PlacedToken] -> Either String Program
parser tokens =
  case runStateT parserWrapper (ParserState {tokens=tokens, range=unknownRange}) of
    Left err -> Left err
    Right (res, _) -> Right res
}
