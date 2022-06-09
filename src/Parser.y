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
%tokentype { (S.Span, L.Token) }
%error { parseError }
%monad { StateT Id.Counter (Either String) }

%token
  bool { (_, L.Bool _) }
  int { (_, L.Int _) }
  float { (_, L.Float _) }
  not { (_, L.Not) }
  '-' { (_, L.Minus) }
  '+' { (_, L.Plus) }
  '-.' { (_, L.MinusDot) }
  '+.' { (_, L.PlusDot) }
  '*.' { (_, L.AstDot) }
  '/.' { (_, L.SlashDot) }
  '=' { (_, L.Equal) }
  '<>' { (_, L.LessGreater) }
  '<=' { (_, L.LessEqual) }
  '>=' { (_, L.GreaterEqual) }
  '<' { (_, L.Less) }
  '>' { (_, L.Greater) }
  if { (_, L.If) }
  then { (_, L.Then) }
  else { (_, L.Else) }
  ident { (_, L.Ident _) }
  let { (_, L.Let) }
  in { (_, L.In) }
  rec { (_, L.Rec) }
  ',' { (_, L.Comma) }
  'Array.create' { (_, L.ArrayCreate) }
  '.' { (_, L.Dot) }
  '<-' { (_, L.LessMinus) }
  ';' { (_, L.Semicolon) }
  '(' { (_, L.LParen) }
  ')' { (_, L.RParen) }

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

ident_ : ident { (case snd $1 of L.Ident x -> x; _ -> undefined) }

-- 括弧をつけなくても関数の引数になれる式
simple_exp : '(' exp ')' { $2 }
           | '(' ')' { S.Unit (fst $1) }
           | bool { S.Bool (fst $1) (case snd $1 of L.Bool x -> x; _ -> undefined) }
           | int { S.Int (fst $1) (case snd $1 of L.Int x -> x; _ -> undefined) }
           | float { S.Float (fst $1) (case snd $1 of L.Float x -> x; _ -> undefined) }
           | ident { S.Var (fst $1) (case snd $1 of L.Ident x -> x; _ -> undefined) }
           | simple_exp '.' '(' exp ')' { S.Get (fst $2) $1 $4 }

app_exp : simple_exp { $1 }
        | simple_exp actual_args { S.App (S.annOf $1) $1 $2 }
        | 'Array.create' simple_exp simple_exp { S.Array (fst $1) $2 $3 }
        | not app_exp { S.Not (fst $1) $2 }

app_exp_right : simple_exp { $1 }
              | simple_exp actual_args { S.App (S.annOf $1) $1 $2 }
              | 'Array.create' simple_exp simple_exp { S.Array (fst $1) $2 $3 }
              | not right_exp(app_exp_right) { S.Not (fst $1) $2 }

neg_exp : app_exp { $1 }
        | '-' neg_exp { case $2 of S.Float _ f -> S.Float (fst $1) (- f) ; e -> S.Neg (fst $1) e }
        | '-.' neg_exp { S.FNeg (fst $1) $2 }

neg_exp_right : app_exp_right { $1 }
              | '-' right_exp(neg_exp_right) { case $2 of S.Float _ f -> S.Float (fst $1) (- f) ; e -> S.Neg (fst $1) e }
              | '-.' right_exp(neg_exp_right) { S.FNeg (fst $1) $2 }

mult_exp : neg_exp { $1 }
         | mult_exp '*.' neg_exp { S.FMul (fst $2) $1 $3 }
         | mult_exp '/.' neg_exp { S.FDiv (fst $2) $1 $3 }

mult_exp_right : neg_exp_right { $1 }
               | mult_exp '*.' right_exp(neg_exp_right) { S.FMul (fst $2) $1 $3 }
               | mult_exp '/.' right_exp(neg_exp_right) { S.FDiv (fst $2) $1 $3 }

add_exp : mult_exp { $1 }
        | add_exp '+' mult_exp { S.Add (fst $2) $1 $3 }
        | add_exp '-' mult_exp { S.Sub (fst $2) $1 $3 }
        | add_exp '+.' mult_exp { S.FAdd (fst $2) $1 $3 }
        | add_exp '-.' mult_exp { S.FSub (fst $2) $1 $3 }

add_exp_right : mult_exp_right { $1 }
              | add_exp '+' right_exp(mult_exp_right) { S.Add (fst $2) $1 $3 }
              | add_exp '-' right_exp(mult_exp_right) { S.Sub (fst $2) $1 $3 }
              | add_exp '+.' right_exp(mult_exp_right) { S.FAdd (fst $2) $1 $3 }
              | add_exp '-.' right_exp(mult_exp_right) { S.FSub (fst $2) $1 $3 }

rel_exp : add_exp { $1 }
        | rel_exp '=' add_exp { S.Eq (fst $2) $1 $3 }
        | rel_exp '<>' add_exp { S.Not (fst $2) (S.Eq (fst $2) $1 $3) }
        | rel_exp '<' add_exp { S.Not (fst $2) (S.LE (fst $2) $3 $1) }
        | rel_exp '>' add_exp { S.Not (fst $2) (S.LE (fst $2) $1 $3) }
        | rel_exp '<=' add_exp { S.LE (fst $2) $1 $3 }
        | rel_exp '>=' add_exp { S.LE (fst $2) $3 $1 }

rel_exp_right : add_exp_right { $1 }
              | rel_exp '=' right_exp(add_exp_right) { S.Eq (fst $2) $1 $3 }
              | rel_exp '<>' right_exp(add_exp_right) { S.Not (fst $2) (S.Eq (fst $2) $1 $3) }
              | rel_exp '<' right_exp(add_exp_right) { S.Not (fst $2) (S.LE (fst $2) $3 $1) }
              | rel_exp '>' right_exp(add_exp_right) { S.Not (fst $2) (S.LE (fst $2) $1 $3) }
              | rel_exp '<=' right_exp(add_exp_right) { S.LE (fst $2) $1 $3 }
              | rel_exp '>=' right_exp(add_exp_right) { S.LE (fst $2) $3 $1 }

tuple_exp : rel_exp { $1 }
          | rel_exp ',' tuple_exp_rest { S.Tuple (fst $2) ($1 : $3) }

tuple_exp_rest : rel_exp { [$1] }
               | rel_exp ',' tuple_exp_rest { $1 : $3 }

tuple_exp_right : rel_exp_right { $1 }
                | rel_exp ',' tuple_exp_right_rest { S.Tuple (fst $2) ($1 : $3) }
                | rel_exp ',' head_exp { S.Tuple (fst $2) [$1, $3] }

tuple_exp_right_rest : rel_exp_right { [$1] }
                     | rel_exp ',' tuple_exp_right_rest { $1 : $3 }
                     | rel_exp ',' head_exp { [$1, $3] }

put_exp : tuple_exp { $1 }
        | put_(put_exp) { $1 }
        | put_(if_(if_exp)) { $1 }

put_exp_right : tuple_exp_right { $1 }
              | put_(put_exp_right) { $1 }
              | put_(if_(if_exp_right)) { $1 }
              | put_(let_(exp)) { $1 }

put_(tail_exp) : simple_exp '.' '(' exp ')' '<-' tail_exp { S.Put (fst $2) $1 $4 $7 }

if_exp : put_exp { $1 }
       | if_(if_exp) { $1 }

if_exp_right : put_exp_right { $1 }
             | if_(if_exp_right) { $1 }
             | if_(let_(exp)) { $1 }

if_(tail_exp) : if exp then exp else tail_exp { S.If (fst $1) $2 $4 $6 }

let_(tail_exp) : let ident_ '=' exp in tail_exp { S.Let (fst $1) (addTyp $2) $4 $6 }
               | let rec fundef in tail_exp { S.LetRec (fst $1) $3 $5 }
               | let '(' pat ')' '=' exp in tail_exp { S.LetTuple (fst $1) $3 $6 $8 }

-- 代入、if, letのいずれかから始まる式
head_exp : put_(put_exp_right) { $1 }
         | if_(if_exp_right) { $1 }
         | let_(exp) { $1 }

right_exp(p) : p { $1 }
             | head_exp { $1 }

-- 一般の式
exp : if_exp_right { $1 }
    | if_exp ';' exp {% do { tmp <- Id.genTmp Type.Unit; return (S.Let (fst $2) (tmp, Type.Unit) $1 $3) } }
    | let_(exp) { $1 }

fundef : ident_ formal_args '=' exp { S.FunDef { S.name = addTyp $1, S.args = $2, S.body = $4 } }

formal_args : ident_ formal_args { addTyp $1 : $2 }
            | ident_ { [addTyp $1] }

actual_args : actual_args simple_exp { $1 ++ [$2] }
            | simple_exp { [$1] }

pat : pat ',' ident_ { $1 ++ [addTyp $3] }
    | ident_ ',' ident_ { [addTyp $1, addTyp $3] }

{
parseError :: [(S.Span, L.Token)] -> StateT Id.Counter (Either String) a
parseError tokens = throwError $ "parse error: " ++ show (take 5 tokens)

addTyp :: a -> (a, Type.TypeF Identity)
addTyp x = (x, Type.Var (Identity Nothing))
}
