module Type where
import           Data.Functor.Classes
import           Data.Functor.Const
import           Data.Void
import           MyPrelude

data TypeF f = Unit
             | Bool
             | Int
             | Float
             | Fun [TypeF f] (TypeF f)
             | Tuple [TypeF f]
             | Array (TypeF f)
             | Var !(f (Maybe (TypeF f)))

type Type = TypeF (Const Void)

instance Eq1 f => Eq (TypeF f) where
  Unit == Unit           = True
  Bool == Bool           = True
  Int == Int             = True
  Float == Float         = True
  Fun xs y == Fun xs' y' = xs == xs' && y == y'
  Tuple xs == Tuple xs'  = xs == xs'
  Array t == Array u     = t == u
  Var f == Var g         = liftEq (==) f g
  _ == _                 = False

instance Show1 f => Show (TypeF f) where
  showsPrec prec x = case x of
                       Unit -> showString "Unit"
                       Bool -> showString "Bool"
                       Int -> showString "Int"
                       Float -> showString "Float"
                       Fun args result -> showParen (prec > 10) $ showString "Fun " . showList args . showChar ' ' . showsPrec 11 result
                       Tuple elements -> showParen (prec > 10) $ showString "Tuple " . showList elements
                       Array element -> showParen (prec > 10) $ showString "Array " . showsPrec 11 element
                       Var f -> showParen (prec > 10) $ showString "Var " . liftShowsPrec showsPrec showList 11 f

mapTypeM :: Applicative m => (f (Maybe (TypeF f)) -> m (TypeF g)) -> TypeF f -> m (TypeF g)
mapTypeM _ Unit       = pure Unit
mapTypeM _ Bool       = pure Bool
mapTypeM _ Int        = pure Int
mapTypeM _ Float      = pure Float
mapTypeM f (Fun xs y) = Fun <$> traverse (mapTypeM f) xs <*> mapTypeM f y
mapTypeM f (Tuple xs) = Tuple <$> traverse (mapTypeM f) xs
mapTypeM f (Array x)  = Array <$> mapTypeM f x
mapTypeM f (Var x)    = f x

-- genTyp is defined in Typing
