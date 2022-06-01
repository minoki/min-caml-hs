{
module Parser where
import qualified Lexer as L
import qualified Syntax as S
import qualified Type
import Data.Functor.Identity
}

%name parseExp exp
%tokentype { L.Token }
%error { parseError }

%token
  bool { L.Bool $$ }
  int { L.Int $$ }
  float { L.Float $$ }
  not { L.Not }
  '-' { L.Minus }
  '+' { L.Plus }
  '-.' { L.MinusDot }
  '+.' { L.PlusDot }
  '*.' { L.AstDot }
  '/.' { L.SlashDot }
  '=' { L.Equal }
  '<>' { L.LessGreater }
  '<=' { L.LessEqual }
  '>=' { L.GreaterEqual }
  '<' { L.Less }
  '>' { L.Greater }
  if { L.If }
  then { L.Then }
  else { L.Else }
  ident { L.Ident $$ }
  let { L.Let }
  in { L.In }
  rec { L.Rec }
  ',' { L.Comma }
  'Array.create' { L.ArrayCreate }
  '.' { L.Dot }
  '<-' { L.LessMinus }
  ';' { L.Semicolon }
  '(' { L.LParen }
  ')' { L.RParen }
  EOF { L.EOF }

%right LET
%right ';'
%right IF
%right '<-'
%left ','
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-' '+.' '-.'
%left '*.' '/.'
%right NEG
%left APP
%left '.'
%%

-- 括弧をつけなくても関数の引数になれる式
simple_exp : '(' exp ')' { $2 }
           | '(' ')' { S.Unit }
           | bool { S.Bool $1 }
           | int { S.Int $1 }
           | float { S.Float $1 }
           | ident { S.Var $1 }
           | simple_exp '.' '(' exp ')' { S.Get $1 $4 }

-- 一般の式
exp : simple_exp { $1 }
    | not exp %prec APP { S.Not $2 }
    | '-' exp %prec NEG { case $2 of S.Float f -> S.Float (- f) ; e -> S.Neg e }
    | exp '+' exp { S.Add $1 $3 }
    | exp '-' exp { S.Sub $1 $3 }
    | exp '=' exp { S.Eq $1 $3 }
    | exp '<>' exp { S.Not (S.Eq $1 $3) }
    | exp '<' exp { S.Not (S.LE $3 $1) }
    | exp '>' exp { S.Not (S.LE $1 $3) }
    | exp '<=' exp { S.LE $1 $3 }
    | exp '>=' exp { S.LE $3 $1 }
    | if exp then exp else exp %prec IF { S.If $2 $4 $6 }
    | '-.' exp %prec NEG { S.FNeg $2 }
    | exp '+.' exp { S.FAdd $1 $3 }
    | exp '-.' exp { S.FSub $1 $3 }
    | exp '*.' exp { S.FMul $1 $3 }
    | exp '/.' exp { S.FDiv $1 $3 }
    | let ident '=' exp in exp %prec LET { S.Let (addTyp $2) $4 $6 }
    | let rec fundef in exp %prec LET { S.LetRec $3 $5 }
    | exp actual_args %prec APP { S.App $1 $2 }
    | elems { S.Tuple $1 }
    | let '(' pat ')' '=' exp in exp %prec LET { S.LetTuple $3 $6 $8 }
    | simple_exp '.' '(' exp ')' '<-' exp { S.Put $1 $4 $7 }
    | exp ';' exp { S.Let ("BOGUS: TODO", Type.Unit) $1 $3 }
    | 'Array.create' simple_exp simple_exp %prec APP { S.Array $2 $3 }

fundef : ident formal_args '=' exp { S.FunDec { S.name = addTyp $1, S.args = $2, S.body = $4 } }

formal_args : ident formal_args { addTyp $1 : $2 }
            | ident { [addTyp $1] }

actual_args : actual_args simple_exp %prec APP { $1 ++ [$2] }
            | simple_exp %prec APP { [$1] }

elems : elems ',' exp { $1 ++ [$3] }
      | exp ',' exp { [$1, $3] }

pat : pat ',' ident { $1 ++ [addTyp $3] }
    | ident ',' ident { [addTyp $1, addTyp $3] }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"

addTyp :: a -> (a, Type.TypeF Identity)
addTyp x = (x, Type.Unit) -- TODO: Use genTyp
}
