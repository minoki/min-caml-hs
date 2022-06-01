module Type where
import Control.Monad.ST
import Data.STRef
import Data.Functor.Classes

data TypeF f = Unit
             | Bool
             | Int
             | Float
             | Fun [TypeF f] (TypeF f)
             | Tuple [TypeF f]
             | Array (TypeF f)
             | Var (f (Maybe (TypeF f)))

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

genTyp :: ST s (TypeF (STRef s))
genTyp = do r <- newSTRef Nothing
            return (Var r)
