{
module Parser where

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace

import Lexer
}

%name parserWrapper
%monad { Parser }
%lexer { lexerWrapper } { TEof }
%tokentype { Token }
%error { parseError }

%token
  id  { TIdentifier $$ }
  int { TInteger $$ }
  def { TKeyword "def" }
  '+' { TOpAdd }
  '\n' {TNewline}
  eof  {TEof}

%%

Prog : LinesExpr           { $1 }
LinesExpr : LineExpr LinesExpr { $1 : $2 }
          | '\n' LinesExpr     { $2 }
          | '\n'               { [] }
LineExpr : Expr '\n'  { $1 }
Frac : int            { AInteger $1 }
Expr : Frac           { $1 }
     | Expr '+' Frac  { AOpAdd $1 $3 }

{
parseError _ = do
  ParserState {range=range} <- get
  fail $ "Parse error " ++ show range

data AValue = AInteger Int | AOpAdd AValue AValue deriving (Show, Eq)

data ParserState = ParserState { tokens :: [PlacedToken], range :: Range } deriving (Show)

type ASTRoot = [AValue]
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
  modify (\s -> ParserState {tokens=rest, range=(betterRange range old_range)})
  cont token

parser :: [PlacedToken] -> Either String ASTRoot
parser tokens =
  case runStateT parserWrapper (ParserState {tokens=tokens, range=unknownRange}) of
    Left err -> Left err
    Right (res, _) -> Right res
}
