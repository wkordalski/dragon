{
module Parser where

import Control.Monad.Except
import Control.Monad.Reader

import Lexer
}

%name parserWrapper
%monad { Syntax }
%lexer { lexerWrapper } { TEof }
%tokentype { Token }
%error { parseError }

%token
  id  { TIdentifier $$ }
  int { TInteger $$ }
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
parseError _ = fail "Parse error"

data AValue = AInteger Int | AOpAdd AValue AValue deriving (Show, Eq)

data SyntaxState = SyntaxState { tokens :: [PlacedToken], range :: Range }

-- type Syntax = [PlacedToken] -> StateT SyntaxState (Either String)

type Syntax ttt = ReaderT SyntaxState (Either String) ttt

--lexer :: (Token -> a) -> String -> a
lexerWrapper :: (Token -> Syntax a) -> Syntax a
lexerWrapper cont = do
  SyntaxState {tokens=((token, range):rest)} <- ask
  local (\_ -> SyntaxState {tokens=rest, range=range}) $ (cont token)


parser tokens =
  runReaderT parserWrapper (SyntaxState {tokens=tokens, range=unknownRange})
}
