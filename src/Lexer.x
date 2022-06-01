{
import qualified Id
import qualified Type
}

%wrapper "monadUserState"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]

tokens :-
  $white+ ;
  \( { token (\input len -> LParen) }
  \) { token (\input len -> RParen) }
  true { token (\input len -> Bool True) }
  false { token (\input len -> Bool False) }
  not { token (\input len -> Not) }
  $digit+ { token (\(_, _, _, s) len -> Int (read (take len s))) }
  $digit+ (\. $digit*)? ([eE] [\+\-]? $digit+)? { token (\(_, _, _, s) len -> Float (read (take len s))) }
  \- { token (\input len -> Minus) }
  \+ { token (\input len -> Plus) }
  \-\. { token (\input len -> MinusDot) }
  \+\. { token (\input len -> PlusDot) }
  \*\. { token (\input len -> AstDot) }
  \/\. { token (\input len -> SlashDot) }
  \= { token (\input len -> Equal) }
  \<\> { token (\input len -> LessGreater) }
  \<\= { token (\input len -> LessEqual) }
  \>\= { token (\input len -> GreaterEqual) }
  \< { token (\input len -> Less) }
  \> { token (\input len -> Greater) }
  if { token (\input len -> If) }
  then { token (\input len -> Then) }
  else { token (\input len -> Else) }
  let { token (\input len -> Let) }
  in { token (\input len -> In) }
  rec { token (\input len -> Rec) }
  \, { token (\input len -> Comma) }
  _ { \input len -> do { state <- alexGetUserState; let { (id, state') = Id.genTmp Type.Unit state }; alexSetUserState state'; return (Ident id) } }
  Array\.create { token (\input len -> ArrayCreate) }
  \. { token (\input len -> Dot) }
  \<\- { token (\input len -> LessMinus) }
  \; { token (\input len -> Semicolon) }
  $lower [$lower $upper $digit \_]* { token (\(_, _, _, s) len -> Ident (take len s)) }

{
type AlexUserState = Int

alexInitUserState :: AlexUserState
alexInitUserState = 0

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

main = do
  s <- getContents
  case runAlex s scanAll of
    Left msg -> putStrLn msg
    Right result -> print result
}
