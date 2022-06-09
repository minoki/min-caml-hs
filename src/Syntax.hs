module Syntax where
import           Data.Functor.Const
import           Data.Void
import           Id
import           MyPrelude
import qualified Type as T

type Exp = ExpF (Const Void)

data ExpF f = Unit
            | Bool Bool
            | Int Int
            | Float Double
            | Not (ExpF f)
            | Neg (ExpF f)
            | Add (ExpF f) (ExpF f)
            | Sub (ExpF f) (ExpF f)
            | FNeg (ExpF f)
            | FAdd (ExpF f) (ExpF f)
            | FSub (ExpF f) (ExpF f)
            | FMul (ExpF f) (ExpF f)
            | FDiv (ExpF f) (ExpF f)
            | Eq (ExpF f) (ExpF f)
            | LE (ExpF f) (ExpF f)
            | If (ExpF f) (ExpF f) (ExpF f)
            | Let (Id, T.TypeF f) (ExpF f) (ExpF f)
            | Var Id
            | LetRec (FunDefF f) (ExpF f)
            | App (ExpF f) [ExpF f]
            | Tuple [ExpF f]
            | LetTuple [(Id, T.TypeF f)] (ExpF f) (ExpF f)
            | Array (ExpF f) (ExpF f)
            | Get (ExpF f) (ExpF f)
            | Put (ExpF f) (ExpF f) (ExpF f)
            deriving Show

data FunDefF f = FunDef { name :: (Id, T.TypeF f)
                        , args :: [(Id, T.TypeF f)]
                        , body :: ExpF f
                        }
                 deriving Show

mapExpM :: Applicative m => (T.TypeF f -> m (T.TypeF g)) -> ExpF f -> m (ExpF g)
mapExpM _ Unit = pure Unit
mapExpM _ (Bool x) = pure (Bool x)
mapExpM _ (Int x) = pure (Int x)
mapExpM _ (Float x) = pure (Float x)
mapExpM f (Not x) = Not <$> mapExpM f x
mapExpM f (Neg x) = Neg <$> mapExpM f x
mapExpM f (Add x y) = Add <$> mapExpM f x <*> mapExpM f y
mapExpM f (Sub x y) = Sub <$> mapExpM f x <*> mapExpM f y
mapExpM f (FNeg x) = FNeg <$> mapExpM f x
mapExpM f (FAdd x y) = FAdd <$> mapExpM f x <*> mapExpM f y
mapExpM f (FSub x y) = FSub <$> mapExpM f x <*> mapExpM f y
mapExpM f (FMul x y) = FMul <$> mapExpM f x <*> mapExpM f y
mapExpM f (FDiv x y) = FDiv <$> mapExpM f x <*> mapExpM f y
mapExpM f (Eq x y) = Eq <$> mapExpM f x <*> mapExpM f y
mapExpM f (LE x y) = LE <$> mapExpM f x <*> mapExpM f y
mapExpM f (If x y z) = If <$> mapExpM f x <*> mapExpM f y <*> mapExpM f z
mapExpM f (Let (x, t) y z) = (\t' -> Let (x, t')) <$> f t <*> mapExpM f y <*> mapExpM f z
mapExpM _ (Var x) = pure (Var x)
mapExpM f (LetRec fundec x) = LetRec <$> mapFunDefM f (mapExpM f) fundec <*> mapExpM f x
mapExpM f (App x ys) = App <$> mapExpM f x <*> traverse (mapExpM f) ys
mapExpM f (Tuple xs) = Tuple <$> traverse (mapExpM f) xs
mapExpM f (LetTuple xts y z) = LetTuple <$> traverse (\(x, t) -> (,) x <$> f t) xts <*> mapExpM f y <*> mapExpM f z
mapExpM f (Array x y) = Array <$> mapExpM f x <*> mapExpM f y
mapExpM f (Get x y) = Get <$> mapExpM f x <*> mapExpM f y
mapExpM f (Put x y z) = Put <$> mapExpM f x <*> mapExpM f y <*> mapExpM f z

mapFunDefM :: Applicative m => (T.TypeF f -> m (T.TypeF g)) -> (ExpF f -> m (ExpF g)) -> FunDefF f -> m (FunDefF g)
mapFunDefM f g (FunDef { name = (x, t), args = yts, body = body }) = (\t' -> FunDef (x, t')) <$> f t <*> traverse (\(y, t) -> (,) y <$> f t) yts <*> g body
