{
module Parser where
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import qualified Id
import qualified Lexer as L
import           Prelude
import qualified Syntax as S
import qualified Type
}

%name parseExp exp
%tokentype { L.Token }
%error { parseError }
%monad { StateT Int (Either String) }

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

{- おまじないには頼らない
%right LET
%right ';'
%right IF
%right '<-'
%right ','
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-' '+.' '-.'
%left '*.' '/.'
%right NEG
%left APP
-}
%%

-- 括弧をつけなくても関数の引数になれる式
simple_exp : '(' exp ')' { $2 }
           | '(' ')' { S.Unit }
           | bool { S.Bool $1 }
           | int { S.Int $1 }
           | float { S.Float $1 }
           | ident { S.Var $1 }
           | simple_exp '.' '(' exp ')' { S.Get $1 $4 }

app_exp : simple_exp { $1 }
        | simple_exp actual_args { S.App $1 $2 }
        | 'Array.create' simple_exp simple_exp { S.Array $2 $3 }
        | not app_exp { S.Not $2 }

app_exp_right : simple_exp { $1 }
              | simple_exp actual_args { S.App $1 $2 }
              | 'Array.create' simple_exp simple_exp { S.Array $2 $3 }
              | not right_exp(app_exp_right) { S.Not $2 }

neg_exp : app_exp { $1 }
        | '-' neg_exp { case $2 of S.Float f -> S.Float (- f) ; e -> S.Neg e }
        | '-.' neg_exp { S.FNeg $2 }

neg_exp_right : app_exp_right { $1 }
              | '-' right_exp(neg_exp_right) { case $2 of S.Float f -> S.Float (- f) ; e -> S.Neg e }
              | '-.' right_exp(neg_exp_right) { S.FNeg $2 }

mult_exp : neg_exp { $1 }
         | mult_exp '*.' neg_exp { S.FMul $1 $3 }
         | mult_exp '/.' neg_exp { S.FDiv $1 $3 }

mult_exp_right : neg_exp_right { $1 }
               | mult_exp '*.' right_exp(neg_exp_right) { S.FMul $1 $3 }
               | mult_exp '/.' right_exp(neg_exp_right) { S.FDiv $1 $3 }

add_exp : mult_exp { $1 }
        | add_exp '+' mult_exp { S.Add $1 $3 }
        | add_exp '-' mult_exp { S.Sub $1 $3 }
        | add_exp '+.' mult_exp { S.FAdd $1 $3 }
        | add_exp '-.' mult_exp { S.FSub $1 $3 }

add_exp_right : mult_exp_right { $1 }
              | add_exp '+' right_exp(mult_exp_right) { S.Add $1 $3 }
              | add_exp '-' right_exp(mult_exp_right) { S.Sub $1 $3 }
              | add_exp '+.' right_exp(mult_exp_right) { S.FAdd $1 $3 }
              | add_exp '-.' right_exp(mult_exp_right) { S.FSub $1 $3 }

rel_exp : add_exp { $1 }
        | rel_exp '=' add_exp { S.Eq $1 $3 }
        | rel_exp '<>' add_exp { S.Not (S.Eq $1 $3) }
        | rel_exp '<' add_exp { S.Not (S.LE $3 $1) }
        | rel_exp '>' add_exp { S.Not (S.LE $1 $3) }
        | rel_exp '<=' add_exp { S.LE $1 $3 }
        | rel_exp '>=' add_exp { S.LE $3 $1 }

rel_exp_right : add_exp_right { $1 }
              | rel_exp '=' right_exp(add_exp_right) { S.Eq $1 $3 }
              | rel_exp '<>' right_exp(add_exp_right) { S.Not (S.Eq $1 $3) }
              | rel_exp '<' right_exp(add_exp_right) { S.Not (S.LE $3 $1) }
              | rel_exp '>' right_exp(add_exp_right) { S.Not (S.LE $1 $3) }
              | rel_exp '<=' right_exp(add_exp_right) { S.LE $1 $3 }
              | rel_exp '>=' right_exp(add_exp_right) { S.LE $3 $1 }

tuple_exp : rel_exp { $1 }
          | rel_exp ',' tuple_exp_rest { S.Tuple ($1 : $3) }

tuple_exp_rest : rel_exp { [$1] }
               | rel_exp ',' tuple_exp_rest { $1 : $3 }

tuple_exp_right : rel_exp_right { $1 }
                | rel_exp ',' tuple_exp_right_rest { S.Tuple ($1 : $3) }
                | rel_exp ',' head_exp { S.Tuple [$1, $3] }

tuple_exp_right_rest : rel_exp_right { [$1] }
                     | rel_exp ',' tuple_exp_right_rest { $1 : $3 }
                     | rel_exp ',' head_exp { [$1, $3] }

put_exp : tuple_exp { $1 }
        | put_(put_exp) { $1 }

put_exp_right : tuple_exp_right { $1 }
              | put_(put_exp_right) { $1 }
              | put_(if_(if_exp_right)) { $1 }
              | put_(let_(exp)) { $1 }

put_(tail_exp) : simple_exp '.' '(' exp ')' '<-' tail_exp { S.Put $1 $4 $7 }

if_exp : put_exp { $1 }
       | if_(if_exp) { $1 }

if_exp_right : put_exp_right { $1 }
             | if_(if_exp_right) { $1 }
             | if_(let_(exp)) { $1 }

if_(tail_exp) : if exp then exp else tail_exp { S.If $2 $4 $6 }

let_(tail_exp) : let ident '=' exp in tail_exp { S.Let (addTyp $2) $4 $6 }
               | let rec fundef in tail_exp { S.LetRec $3 $5 }
               | let '(' pat ')' '=' exp in tail_exp { S.LetTuple $3 $6 $8 }

-- 代入、if, letのいずれかから始まる式
head_exp : put_(put_exp_right) { $1 }
         | if_(if_exp_right) { $1 }
         | let_(exp) { $1 }

right_exp(p) : p { $1 }
             | head_exp { $1 }

-- 一般の式
exp : if_exp_right { $1 }
    | if_exp ';' exp {% do { tmp <- genTmp Type.Unit; return (S.Let (tmp, Type.Unit) $1 $3) } }
    | let_(exp) { $1 }

fundef : ident formal_args '=' exp { S.FunDef { S.name = addTyp $1, S.args = $2, S.body = $4 } }

formal_args : ident formal_args { addTyp $1 : $2 }
            | ident { [addTyp $1] }

actual_args : actual_args simple_exp { $1 ++ [$2] }
            | simple_exp { [$1] }

pat : pat ',' ident { $1 ++ [addTyp $3] }
    | ident ',' ident { [addTyp $1, addTyp $3] }

{
parseError :: [L.Token] -> StateT Int (Either String) a
parseError _ = throwError "parse error"

genTmp :: Type.TypeF f -> StateT Int (Either String) Id.Id
genTmp t = state (Id.genTmp t)

addTyp :: a -> (a, Type.TypeF Identity)
addTyp x = (x, Type.Var (Identity Nothing))
}
