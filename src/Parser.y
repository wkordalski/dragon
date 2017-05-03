{
module Parser where

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace

import Ast
import Lexer
}

%name parserWrapper
%name parserExprWrapper ProgramExpr
%monad { Parser }
%lexer { lexerWrapper } { TEof }
%tokentype { Token }
%error { parseError }

%token
  id      { TIdentifier $$ }
  integer { TInteger $$ }

  int     { TKeyword "int" }
  bool    { TKeyword "bool" }
  ptr     { TKeyword "ptr" }

  def     { TKeyword "def" }
  var     { TKeyword "var" }
  return  { TKeyword "return" }

  if      { TKeyword "if" }
  ei      { TKeyword "ei" }
  else    { TKeyword "else" }
  then    { TKeyword "then" }

  while   { TKeyword "while" }
  pass    { TKeyword "pass" }

  true    { TKeyword "true" }
  false   { TKeyword "false" }
  not     { TKeyword "not" }
  and     { TKeyword "and" }
  or      { TKeyword "or" }

  '='     { TOperator "="}
  '+'     { TOperator "+" }
  '-'     { TOperator "-" }
  '*'     { TOperator "*" }
  '/'     { TOperator "/" }
  '//'    { TOperator "//" }
  '%'     { TOperator "%" }
  '~'     { TOperator "~" }
  '!'     { TOperator "!" }
  '&'     { TOperator "&" }
  '::'    { TOperator "::" }
  ','     { TOperator "," }
  '.'     { TOperator "." }
  '**'    { TOperator "**" }
  '=='    { TOperator "==" }
  '!='    { TOperator "!=" }
  '<'     { TOperator "<" }
  '>'     { TOperator ">" }
  '<='    { TOperator "<=" }
  '>='    { TOperator ">=" }
  '\\'    { TOperator "\\" }
  '->'    { TOperator "->" }
  '+='    { TOperator "+=" }
  '*='    { TOperator "*=" }

  '('     { TRParenO }
  ')'     { TRParenC }

  '\n'    { TNewline }
  '\>'    { TIndent }
  '\<'    { TDedent }
  eof     { TEof }

%%

Program : Declarations           { $1 }
ProgramExpr : Expr Newlines      { $1 }

Newlines : '\n'                   { () }
Newlines : '\n' Newlines          { () }

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
Statement : def id ArgsPatternMatch '::' TypeExpr '=' Expr '\n'                   { SFunction $2 $5 $3 [SReturn $7] }
Statement : def id ArgsPatternMatch '\n' '\>' '::' TypeExpr '\n' Statements '\<'  { SFunction $2 $7 $3 $9 }
Statement : IfIf IfEiMany IfElseMaybe             { SIf ($1:$2) $3 }
Statement : while Expr '\n' '\>' Statements '\<'  { SWhile $2 $5 }
Statement : while Expr ',' Statement              { SWhile $2 [$4] }
Statement : pass '\n'                             { SPass }

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


Expr : Expr14              { $1 }

Expr0 :: { Expr }
Expr0 : id                { EVariable $1 }
      | integer           { EInteger $1 }
      | true              { EBoolean True }
      | false             { EBoolean False }
      | '(' ')'           { ENone }
      | '(' Expr ')'      { $2 }
      | '(' Expr TupleRest ')'  { ETuple ($2 : $3 ) }

TupleRest : ',' Expr            { [$2] }
          | ',' Expr TupleRest  { $2 : $3 }

Expr1 : Expr1 '.' id      { EMember $1 $3 }
      | Expr0             { $1 }

Expr2 : '!' Expr2         { EDereference $2 }
      | '&' Expr2         { EAddress $2 }
      | Expr1             { $1 }

Expr3 : Expr3 Expr2       { ECall $1 $2 }
      | Expr2             { $1 }


Expr3a : Expr3 '**' Expr3a  { EOpPower $1 $3 }
Expr3a : Expr3             { $1 }

Expr4 : '+' Expr4         { EUOpPlus $2 }
      | '-' Expr4         { EUOpMinus $2 }
      | Expr3a            { $1 }

Expr5 : Expr5 '*' Expr4   { EOpMultiply $1 $3 }
      | Expr5 '/' Expr4   { EOpDivision $1 $3 }
      | Expr5 '%' Expr4   { EOpModulo $1 $3 }
      | Expr4             { $1 }

Expr6 : Expr6 '+' Expr5   { EOpAdd $1 $3 }
      | Expr6 '-' Expr5   { EOpSubtract $1 $3 }
      | Expr5             { $1 }

Expr7 : Expr6             { $1 }

Expr8 : Expr8 '==' Expr7  { EOpEqual $1 $3 }
      | Expr8 '!=' Expr7  { EOpNotEqual $1 $3 }
      | Expr8 '<' Expr7   { EOpLessThan $1 $3 }
      | Expr8 '<=' Expr7  { EOpLessEqualThan $1 $3 }
      | Expr8 '>' Expr7   { EOpGreaterThan $1 $3 }
      | Expr8 '>=' Expr7  { EOpGreaterEqualThan $1 $3 }
      | Expr7             { $1 }

Expr9 : '\\' ArgsPatternMatch '(' '::' TypeExpr ')' '->' Expr    { ELambda $5 $2 $8 }
      | Expr8                                           { $1 }

Expr10 : Expr10 '=' Expr9   { EOpAssign $1 $3 }
       | Expr10 '+=' Expr9  { EOpAssignAdd $1 $3 }
       | Expr10 '*=' Expr9  { EOpAssignMultiply $1 $3 }
       | Expr9              { $1 }

Expr11 : not Expr10         { EOpNegation $2 }
       | Expr10             { $1 }

Expr12 : Expr12 and Expr11  { EOpConjunction $1 $3 }
       | Expr11             { $1 }

Expr13 : Expr13 or Expr12   { EOpAlternative $1 $3 }
       | Expr12             { $1 }

Expr14 : if Expr14 then Expr14 else Expr14  { EIf $2 $4 $6 }
       | Expr13                             { $1 }

ArgsPatternMatch : {- empty -}                    { [] }
ArgsPatternMatch : PatternMatch ArgsPatternMatch  { $1 : $2 }

PatternMatch : id         { PNamed $1 }
             | '(' PatternMatch TuplePmatchRest ')' { PTuple ($2 : $3) }
             | '(' ')'                              { PVoid }

TuplePmatchRest : ',' PatternMatch        { [$2] }
                | ',' PatternMatch TuplePmatchRest { $2 : $3 }

-- add function type
TypeExpr :: { TypeExpr }
TypeExpr : TypeExpr2              { $1 }
TypeExpr0 : id                    { TNamed $1 }
          | int                   { TInt }
          | bool                  { TBool }
          | '(' ')'               { TVoid }
          | '(' TypeExpr ')'      { $2 }
          | '(' TypeExpr TupleTypeExprRest ')' { TTuple $ $2:$3 }

TupleTypeExprRest : ',' TypeExpr    { [$2] }
                  | ',' TypeExpr TupleTypeExprRest { $2 : $3 }


TypeExpr1 : ptr TypeExpr1             { TPointer $2 }
          | TypeExpr0                 { $1 }

TypeExpr2 : TypeExpr1 '->' TypeExpr2  { TFunction $1 $3 }
          | TypeExpr1                 { $1 }

{

parseError _ = do
  ParserState {range=range} <- get
  throwError $ "Parse error " ++ show range


data ParserState = ParserState { tokens :: [PlacedToken], range :: Range } deriving (Show)

type Parser ttt = StateT ParserState (Except String) ttt

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

parserWrapperWrapper pw tokens =
  case runExcept (runStateT pw (ParserState {tokens=tokens, range=unknownRange})) of
    Left err -> throwError err
    Right (res, _) -> return res

parser :: [PlacedToken] -> Except String Program
parser = parserWrapperWrapper parserWrapper

parserExpr :: [PlacedToken] -> Except String Expr
parserExpr = parserWrapperWrapper parserExprWrapper
}
