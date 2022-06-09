{
module Lexer where
import           Control.Monad.State.Strict
import           Data.Char (chr)
import qualified Id
import           MyPrelude hiding (getChar)
import           Syntax (Span(..), nullSpan)
import qualified Type
}

%wrapper "monadUserState"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]

tokens :-
  $white+ ;
  "(*" { comment }
  \( { simpleToken LParen }
  \) { simpleToken RParen }
  true { simpleToken (Bool True) }
  false { simpleToken (Bool False) }
  not { simpleToken Not }
  $digit+ { token (\(posn, _, _, s) len -> mkToken posn len (Int (read (take len s)))) }
  $digit+ \. ([eE] [\+\-]? $digit+)? { token (\(posn, _, _, s) len -> mkToken posn len (Float (read (filter (/= '.') $ take len s)))) }
  $digit+ [eE] [\+\-]? $digit+ { token (\(posn, _, _, s) len -> mkToken posn len (Float (read (take len s)))) }
  $digit+ \. $digit+ ([eE] [\+\-]? $digit+)? { token (\(posn, _, _, s) len -> mkToken posn len (Float (read (take len s)))) }
  \- { simpleToken Minus }
  \+ { simpleToken Plus }
  \-\. { simpleToken MinusDot }
  \+\. { simpleToken PlusDot }
  \*\. { simpleToken AstDot }
  \/\. { simpleToken SlashDot }
  \= { simpleToken Equal }
  \<\> { simpleToken LessGreater }
  \<\= { simpleToken LessEqual }
  \>\= { simpleToken GreaterEqual }
  \< { simpleToken Less }
  \> { simpleToken Greater }
  if { simpleToken If }
  then { simpleToken Then }
  else { simpleToken Else }
  let { simpleToken Let }
  in { simpleToken In }
  rec { simpleToken Rec }
  \, { simpleToken Comma }
  _ { \(posn, _, _, _) len -> mkToken posn len . Ident <$> genTmp }
  Array\.create { simpleToken ArrayCreate }
  Array\.make { simpleToken ArrayCreate }
  \. { simpleToken Dot }
  \<\- { simpleToken LessMinus }
  \; { simpleToken Semicolon }
  $lower [$lower $upper $digit \_]* { token (\(posn, _, _, s) len -> mkToken posn len (Ident (take len s))) }

{
type AlexUserState = Id.Counter

alexInitUserState :: AlexUserState
alexInitUserState = Id.initialCounter

mkToken :: AlexPosn -> Int -> Token -> (Span, Token)
mkToken (AlexPn _absOffset line column) len tok = (Span line column len, tok)

simpleToken :: Token -> AlexInput -> Int -> Alex (Span, Token)
simpleToken tok = \(posn, _prev, _rest, _s) len -> return $ mkToken posn len tok

genTmp :: Alex Id.Id
genTmp = do state <- alexGetUserState
            let (id, idCounter') = runState (Id.genTmp Type.Unit) state
            alexSetUserState idCounter'
            return id

getChar :: AlexInput -> Maybe (Char, AlexInput)
getChar input = case alexGetByte input of
                  Just (c, input') -> Just (chr (fromIntegral c), input')
                  Nothing -> Nothing

comment :: AlexInput -> Int -> Alex (Span, Token)
comment _ _ = do
  input <- alexGetInput
  go (1 :: Int) input
  where
    go 0 input = do alexSetInput input; alexMonadScan
    go n input = do
      case getChar input of
        Nothing -> err input
        Just ('*', input') -> case getChar input' of
                                Nothing -> err input'
                                Just (')', input'') -> go (n - 1) input''
                                Just ('*', _input) -> go n input'
                                Just (_, input'') -> go n input''
        Just ('(', input') -> case getChar input' of
                                Nothing -> err input'
                                Just ('*', input'') -> go (n + 1) input''
                                Just (_, _input) -> go n input'
        Just (_, input') -> go n input'
    err input = do alexSetInput input
                   alexError "unterminated comment."

data Token = Bool Bool
           | Int Int
           | Float Double
           | Not
           | Minus
           | Plus
           | MinusDot
           | PlusDot
           | AstDot
           | SlashDot
           | Equal
           | LessGreater
           | LessEqual
           | GreaterEqual
           | Less
           | Greater
           | If
           | Then
           | Else
           | Ident Id.Id
           | Let
           | In
           | Rec
           | Comma
           | ArrayCreate
           | Dot
           | LessMinus
           | Semicolon
           | LParen
           | RParen
           | EOF
           deriving Show

alexEOF :: Alex (Span, Token)
alexEOF = return (nullSpan, EOF)

scanAll :: Alex [(Span, Token)]
scanAll = do
  let loop revTokens = do tok <- alexMonadScan
                          case tok of
                            (_, EOF) -> return (reverse revTokens)
                            x -> loop (x : revTokens)
  loop []

scanAllAndState :: Alex ([(Span, Token)], AlexUserState)
scanAllAndState = do tokens <- scanAll
                     state <- alexGetUserState
                     return (tokens, state)
}
