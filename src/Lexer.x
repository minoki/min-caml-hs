{
module Lexer where
import Prelude hiding (getChar)
import Data.Char
import qualified Id
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
  $digit+ { token (\(_, _, _, s) len -> Int (read (take len s))) }
  $digit+ (\. $digit*)? ([eE] [\+\-]? $digit+)? { token (\(_, _, _, s) len -> Float (read (take len s))) }
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
  _ { \input len -> Ident <$> genTmp }
  Array\.create { simpleToken ArrayCreate }
  \. { simpleToken Dot }
  \<\- { simpleToken LessMinus }
  \; { simpleToken Semicolon }
  $lower [$lower $upper $digit \_]* { token (\(_, _, _, s) len -> Ident (take len s)) }

{
data AlexUserState = AlexUserState { idCounter :: Int
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { idCounter = 0 }

simpleToken :: Token -> AlexInput -> Int -> Alex Token
simpleToken tok = \input len -> return tok

genTmp :: Alex Id.Id
genTmp = do state <- alexGetUserState
            let (id, idCounter') = Id.genTmp Type.Unit (idCounter state)
            alexSetUserState (state { idCounter = idCounter' })
            return id

getChar :: AlexInput -> Maybe (Char, AlexInput)
getChar input = case alexGetByte input of
                  Just (c, input') -> Just (chr (fromIntegral c), input')
                  Nothing -> Nothing

comment :: AlexInput -> Int -> Alex Token
comment _ _ = do
  input <- alexGetInput
  go 1 input
  where
    go 0 input = do alexSetInput input; alexMonadScan
    go n input = do
      case getChar input of
        Nothing -> err input
        Just ('*', input') -> case getChar input' of
                                Nothing -> err input'
                                Just (')', input'') -> go (n - 1) input''
                                Just ('*', input'') -> go n input'
                                Just (_, input'') -> go n input''
        Just ('(', input') -> case getChar input' of
                                Nothing -> err input'
                                Just ('*', input'') -> go (n + 1) input''
                                Just (_, input'') -> go n input'
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

alexEOF :: Alex Token
alexEOF = return EOF

scanAll = do
  let loop revTokens = do tok <- alexMonadScan
                          case tok of
                            EOF -> return (reverse revTokens)
                            x -> loop (x : revTokens)
  loop []
}
