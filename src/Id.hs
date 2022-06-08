module Id (Id, Label(..), Counter, initialCounter, HasCounter(..), genId, genTmp) where
import           Control.Monad.State.Class
import           Lens.Micro (Lens')
import           Lens.Micro.Mtl (assign, use)
import           MyPrelude
import           Type

type Id = String

newtype Label = Label String deriving (Eq, Show)

newtype Counter = Counter Int

initialCounter :: Counter
initialCounter = Counter 0

class HasCounter s where
  counter :: Lens' s Counter

instance HasCounter Counter where
  counter = id

genId :: (MonadState s m, HasCounter s) => String -> m Id
genId s = do Counter c <- use counter
             let c' = c + 1
             assign counter (Counter c')
             pure $ s ++ "." ++ show c'

idOfType :: TypeF f -> String
idOfType Unit      = "u"
idOfType Bool      = "b"
idOfType Int       = "i"
idOfType Float     = "d"
idOfType (Fun _ _) = "f"
idOfType (Tuple _) = "t"
idOfType (Array _) = "a"
idOfType (Var _)   = error "idOfType var"

genTmp :: (MonadState s m, HasCounter s) => TypeF f -> m Id
genTmp typ = do Counter c <- use counter
                let c' = c + 1
                assign counter (Counter c')
                pure $ "T" ++ idOfType typ ++ show c'
