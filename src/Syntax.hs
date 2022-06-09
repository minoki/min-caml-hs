module Syntax where
import           Data.Functor.Const
import           Data.Void
import           Id
import           MyPrelude
import qualified Type as T

data Span = Span { srcLine :: !Int, srcColumn :: !Int, srcLength :: !Int } deriving Show

nullSpan :: Span
nullSpan = Span (-1) (-1) (-1)

type Ann = Span

type Exp = ExpF (Const Void)

data ExpF f = Unit Ann
            | Bool Ann Bool
            | Int Ann Int
            | Float Ann Double
            | Not Ann (ExpF f)
            | Neg Ann (ExpF f)
            | Add Ann (ExpF f) (ExpF f)
            | Sub Ann (ExpF f) (ExpF f)
            | FNeg Ann (ExpF f)
            | FAdd Ann (ExpF f) (ExpF f)
            | FSub Ann (ExpF f) (ExpF f)
            | FMul Ann (ExpF f) (ExpF f)
            | FDiv Ann (ExpF f) (ExpF f)
            | Eq Ann (ExpF f) (ExpF f)
            | LE Ann (ExpF f) (ExpF f)
            | If Ann (ExpF f) (ExpF f) (ExpF f)
            | Let Ann (Id, T.TypeF f) (ExpF f) (ExpF f)
            | Var Ann Id
            | LetRec Ann (FunDefF f) (ExpF f)
            | App Ann (ExpF f) [ExpF f]
            | Tuple Ann [ExpF f]
            | LetTuple Ann [(Id, T.TypeF f)] (ExpF f) (ExpF f)
            | Array Ann (ExpF f) (ExpF f)
            | Get Ann (ExpF f) (ExpF f)
            | Put Ann (ExpF f) (ExpF f) (ExpF f)
            deriving Show

data FunDefF f = FunDef { name :: (Id, T.TypeF f)
                        , args :: [(Id, T.TypeF f)]
                        , body :: ExpF f
                        }
                 deriving Show

mapExpM :: Applicative m => (T.TypeF f -> m (T.TypeF g)) -> ExpF f -> m (ExpF g)
mapExpM _ (Unit a) = pure $ Unit a
mapExpM _ (Bool a x) = pure $ Bool a x
mapExpM _ (Int a x) = pure $ Int a x
mapExpM _ (Float a x) = pure $ Float a x
mapExpM f (Not a x) = Not a <$> mapExpM f x
mapExpM f (Neg a x) = Neg a <$> mapExpM f x
mapExpM f (Add a x y) = Add a <$> mapExpM f x <*> mapExpM f y
mapExpM f (Sub a x y) = Sub a <$> mapExpM f x <*> mapExpM f y
mapExpM f (FNeg a x) = FNeg a <$> mapExpM f x
mapExpM f (FAdd a x y) = FAdd a <$> mapExpM f x <*> mapExpM f y
mapExpM f (FSub a x y) = FSub a <$> mapExpM f x <*> mapExpM f y
mapExpM f (FMul a x y) = FMul a <$> mapExpM f x <*> mapExpM f y
mapExpM f (FDiv a x y) = FDiv a <$> mapExpM f x <*> mapExpM f y
mapExpM f (Eq a x y) = Eq a <$> mapExpM f x <*> mapExpM f y
mapExpM f (LE a x y) = LE a <$> mapExpM f x <*> mapExpM f y
mapExpM f (If a x y z) = If a <$> mapExpM f x <*> mapExpM f y <*> mapExpM f z
mapExpM f (Let a (x, t) y z) = (\t' -> Let a (x, t')) <$> f t <*> mapExpM f y <*> mapExpM f z
mapExpM _ (Var a x) = pure $ Var a x
mapExpM f (LetRec a fundec x) = LetRec a <$> mapFunDefM f (mapExpM f) fundec <*> mapExpM f x
mapExpM f (App a x ys) = App a <$> mapExpM f x <*> traverse (mapExpM f) ys
mapExpM f (Tuple a xs) = Tuple a <$> traverse (mapExpM f) xs
mapExpM f (LetTuple a xts y z) = LetTuple a <$> traverse (\(x, t) -> (,) x <$> f t) xts <*> mapExpM f y <*> mapExpM f z
mapExpM f (Array a x y) = Array a <$> mapExpM f x <*> mapExpM f y
mapExpM f (Get a x y) = Get a <$> mapExpM f x <*> mapExpM f y
mapExpM f (Put a x y z) = Put a <$> mapExpM f x <*> mapExpM f y <*> mapExpM f z

mapFunDefM :: Applicative m => (T.TypeF f -> m (T.TypeF g)) -> (ExpF f -> m (ExpF g)) -> FunDefF f -> m (FunDefF g)
mapFunDefM f g (FunDef { name = (x, t), args = yts, body = body }) = (\t' -> FunDef (x, t')) <$> f t <*> traverse (\(y, t) -> (,) y <$> f t) yts <*> g body

annOf :: ExpF f -> Ann
annOf (Unit a) = a
annOf (Bool a _) = a
annOf (Int a _) = a
annOf (Float a _) = a
annOf (Neg a _) = a
annOf (Add a _ _) = a
annOf (Sub a _ _) = a
annOf (FNeg a _) = a
annOf (FAdd a _ _) = a
annOf (FSub a _ _) = a
annOf (FMul a _ _) = a
annOf (FDiv a _ _) = a
annOf (Eq a _ _) = a
annOf (LE a _ _) = a
annOf (If a _ _ _) = a
annOf (Let a _ _ _) = a
annOf (Var a _) = a
annOf (LetRec a _ _) = a
annOf (App a _ _) = a
annOf (Tuple a _) = a
annOf (LetTuple a _ _ _) = a
annOf (Array a _ _) = a
annOf (Get a _ _) = a
annOf (Put a _ _ _) = a
