module Lexer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Char as Ch

import Debug.Trace

data Token = TIdentifier String
           | TInteger Int
           | TFloat Float
           | TKTrue
           | TKFalse
           | TKIf
           | TKEi
           | TKElse
           | TIndent
           | TDedent
           | TNewline
           | TEof
           | TRParenO
           | TRParenC
           | TSParenO
           | TSParenC
           | TCParenO
           | TCParenC
           | TOpAdd
           deriving (Eq, Show, Ord)

newtype Place = Place (Int, Int, Int) deriving (Eq, Ord)
unknownPlace = Place(-1, -1, -1)
newtype Range = Range (Place, Place) deriving (Eq, Ord)
unknownRange = Range(unknownPlace, unknownPlace)
type PlacedChar = (Char, Place)

basicPlaceShow (Place (r, c, _)) =
  let qm i = if i == -1 then "?" else show i in
  qm r ++ ":" ++ qm c

instance Show Place where
  show p = "(" ++ basicPlaceShow p ++ ")"

instance Show Range where
  show (Range (b, e)) =
    "(" ++ basicPlaceShow b ++ " - " ++ basicPlaceShow e ++ ")"

type PlacedString = [PlacedChar]
type PlacedToken = (Token, Range)
data Parens = Round | Square | Curly deriving (Eq, Show, Ord)
data CommentOpts = SingleLine | Multiline | Nested Int deriving (Eq, Show, Ord)

data LexerState = LexerState {
  script :: [(Int, PlacedString)],
  line :: PlacedString,
  indents :: [Int],
  parens :: [Parens]
} deriving (Eq, Ord, Show)

makeLexerState l = LexerState {
  script = l,
  line = [],
  indents = [0],
  parens = []
}

failPos k s = do
  Place (r, c, _) <- getSourcePosition
  let kl = length k
  let k' = if kl >= 10 then k else replicate (10 - kl) ' ' ++ k
  fail $ "(" ++ show r ++ ":" ++ show c ++ ") " ++ k' ++ ": " ++ s

failPL = failPos "lexer"

-- instance Except LexerError where
--   noMsg  = LexerInternalError "Unknown internal error"
--   strMsg = LexerInternalError



type Lexer = StateT LexerState (Either String)



-- lexer :: [Char] -> [Token]
lexer l =
  let l1 = runReader (addEndingNewline l) False in
  let pl = runReader (addPlace l1) (Place (1,0, 0)) in
  let nl = skipEmptyLines pl in
  let ll = splitIntoLines nl in
  let mi = map measureIndent ll in
  let dn = runStateT lexAll (makeLexerState mi) in
  case dn of
    Left err -> Left err
    Right (tok, _) -> Right $ tok ++ [(TEof, Range(pos, pos))] where
      pos = Place(1 + length (filter (== '\n') l), 0, length l)


lexAll :: Lexer [PlacedToken]
lexAll = do
  LexerState{script=script} <- get
  case script of
    [] -> return []
    (i, h):t -> do
      -- parse indentation
      indt <- lexIndentation i
      modify (\s@LexerState{script=(_, l):t} -> s {script=t, line=l})
      lint <- lexLine
      rest <- lexAll
      return $ indt ++ lint ++ [(TNewline, Range (Place (-1, -1, -1), Place (-1, -1, -1)))] ++ rest


getSourcePosition = do
  LexerState {script=script, line=line} <- get
  case line of
    [] -> case script of
      (_, (_, Place (r, _, p)):_):_ -> return $ Place (r, 0, p)
      _ -> return $ Place (-1, -1, -1)
    (_, p):_ -> return p

placesToRange :: [PlacedChar] -> Range
placesToRange [] = Range (Place (-1, -1, -1), Place (-1, -1, -1))
placesToRange l = Range (snd $ head l, snd $ last l)

lexIndentation :: Int -> Lexer [PlacedToken]
lexIndentation i = do
  LexerState {indents=ind} <- get
  let (h:t) = ind
  if i > h then do
    modify (\s -> s {indents=i:ind})
    pos <- getSourcePosition
    return [(TIndent, Range (pos, pos))]
  else do
    let (reduced, rest@(h:_)) = span (>i) ind
    if h /= i then failPL "Indentation not matching!"
    else do
      modify (\s -> s {indents=rest})
      pos <- getSourcePosition
      return $ replicate (length reduced) (TDedent, Range (pos, pos))

lexLine :: Lexer [PlacedToken]
lexLine = do
  -- Parse first token, spaces after and
  -- letter -> kwd / identifier
  -- punctuation -> operator
  -- digit -> number
  -- apostrophe/quot -> char/string literal
  -- # /* /# - comments
  LexerState {line=line, parens=parens} <- get
  case line of
    [] -> if null parens then return [] else do
      modify (\s@LexerState{script=(_, l):t} -> s {script=t, line=l})
      lexLine
    h@(c, _):t  | isLetter h -> do
                    tok <- readIdentifierOrKeyword
                    skipSpaces
                    rest <- lexLine
                    return $ tok ++ rest
                | isDigit h -> do
                    tok <- readNumber
                    skipSpaces
                    rest <- lexLine
                    return $ tok ++ rest
                | isPunctuation h -> do
                    tok <- readOperator -- also comments and parens
                    skipSpaces
                    rest <- lexLine
                    return $ tok ++ rest
                | c == '\'' -> failPL "Character literals not supported"
                | c == '"' -> failPL "String literals not supported"
                | otherwise -> failPL $ "Unexpected character " ++ show h

readIdentifierOrKeyword = do
  LexerState {line=line} <- get
  let (tt, rest) = span isIdentifierChar line
  modify (\s -> s {line=rest})
  return [(TIdentifier $ map fst tt, placesToRange tt)]

readNumber = do
  LexerState {line=line} <- get
  let (tt, rest) = span isNumberChar line
  let range = placesToRange tt
  let unplace (c, _) = c
  let numbers = map unplace tt
  let numbers' = filter (\c -> (c == '.') || Ch.isDigit c) numbers
  case length $ filter (== '.') numbers' of
    0 -> do
      modify (\s -> s {line=rest})
      return [(TInteger (read numbers'), range)]
    1 | last numbers' == '.' ->
          failPL "Floating-point number must not end with dot"
      | otherwise-> do
          modify (\s -> s {line=rest})
          return [(TFloat (read numbers'), range)]
    _ -> failPL "Unrecognised number format!"


readOperator = do
  LexerState {line=line} <- get
  let (tt, rest) = span isPunctuation line
  let range = placesToRange tt
  let t = map fst tt
  if startswith "#" t then do
    modify (\s -> s {line=[]})
    return []
  else
    case t of
      "+" -> do
        modify (\s -> s {line=rest})
        return [(TOpAdd, range)]
      _ -> failPL $ "Unknown operator " ++ show t
  where
    startswith [] _ = True
    startswith (h:t) [] = False
    startswith (h:t) (k:l) = (h == k) && startswith t l

skipSpaces :: Lexer ()
skipSpaces = modify helper where
  helper s@LexerState{line=line} =
    let (_, rest) = span isSpace line in
    s {line=rest}


isNewLine (c, _) = c == '\n'
isSpace (c, _) = (c == ' ') || (c == '\t')
isDigit (c, _) = (c >= '0') && (c <= '9')
isVLetter (c, _) = Ch.isAlpha c
isLetter l@(c, _) = (c == '_') || isVLetter l
isAlphaNum l = isDigit l || isLetter l
isPunctuation (c, _) = (c == '+') || (c == '-') || (c == '#')
isIdentifierChar l@(c, _) = isAlphaNum l || c == '\''
isNumberChar l@(c, _) = isDigit l || c == '_' || c == '\'' || c == '.'

spaceWidth (c, _) =
  case c of
    ' ' -> 1
    '\t' -> 2

-- ASSIGN POSITION TO CHARACTERS

addPlace :: String -> Reader Place [PlacedChar]
addPlace [] = return []
addPlace (h:t) = do
  Place (r, c, p) <- ask
  if h == '\n' then do
    rest <- local (\(Place (r, c, p)) ->  Place (r+1, 0, p+1)) $ addPlace t
    return $ (h, Place (r+1, 0, p)) : rest
  else do
    rest <- local (\(Place (r, c, p)) -> Place (r, c+1, p+1)) $ addPlace t
    return $ (h, Place (r, c+1, p)) : rest

-- FILE *MUST* END WITH END-OF-LINE

addEndingNewline :: String -> Reader Bool String
addEndingNewline [] = do
  w <- ask
  if w then return []
  else return ['\n']
addEndingNewline (h:t) =
  if h == '\n' then do
    rest <- local (const True) $ addEndingNewline t
    return $ h : rest
  else do
    rest <- local (const False) $ addEndingNewline t
    return $ h : rest

-- SKIP EMPTY LINES

skipEmptyLines :: PlacedString -> PlacedString
skipEmptyLines [] = []
skipEmptyLines (h:t) =
  if isNewLine h then
    let (spaces, rest) = span isSpace t in
    if not (null rest) && isNewLine (head rest) then
      skipEmptyLines rest
    else
      h : (spaces ++ skipEmptyLines rest)
    -- TAKE SPACES TO NEWLINE THEN DROP
  else
    h : skipEmptyLines t

-- SPLIT INTO LINES

splitIntoLines :: PlacedString -> [PlacedString]
splitIntoLines [] = [[]]
splitIntoLines l =
  let (a, b) = break isNewLine l in
  case b of
    [] -> [a]
    _:r -> a : splitIntoLines r

measureIndent :: PlacedString -> (Int, PlacedString)
measureIndent l =
  let (a, b) = span isSpace l in
  (sum $ map spaceWidth a, b)
