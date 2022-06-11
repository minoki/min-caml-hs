module Inline where
import qualified Alpha
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Id (Id)
import qualified Id
import           KNormal (Exp (..), FunDef (..))
import           Lens.Micro.Mtl (assign, use)
import           Logging
import           MyPrelude
import qualified Type

type M m = ReaderT Int (StateT Id.Counter m) -- threshold

size :: Exp -> Int
size (IfEq _ _ e1 e2)                   = 1 + size e1 + size e2
size (IfLE _ _ e1 e2)                   = 1 + size e1 + size e2
size (Let _ e1 e2)                      = 1 + size e1 + size e2
size (LetRec (FunDef { body = e1 }) e2) = 1 + size e1 + size e2
size (LetTuple _ _ e)                   = 1 + size e
size _                                  = 1

g :: MonadLogger m => Map.Map Id ([(Id, Type.Type)], Exp) -> Exp -> M m Exp
g env (IfEq x y e1 e2) = IfEq x y <$> g env e1 <*> g env e2
g env (IfLE x y e1 e2) = IfLE x y <$> g env e1 <*> g env e2
g env (Let xt e1 e2) = Let xt <$> g env e1 <*> g env e2
g env (LetRec (FunDef { name = (x, t), args = yts, body = e1 }) e2)
  = do threshold <- ask
       let env' = if size e1 > threshold then env else Map.insert x (yts, e1) env
       e1' <- g env' e1
       LetRec (FunDef { name = (x, t), args = yts, body = e1' }) <$> g env' e2
g env (App x ys) | Just (zs, e) <- Map.lookup x env = do
                     putLogLn $ "inlining " ++ x
                     let env' = List.foldl' (\env' ((z, _), y) -> Map.insert z y env') Map.empty (zip zs ys)
                     lift . StateT $ \s -> pure $ runState (Alpha.g env' e) s
g env (LetTuple xts y e) = LetTuple xts y <$> g env e
g _ e = pure e

f :: (MonadLogger m, MonadState s m, Id.HasCounter s) => Exp -> Int -> m Exp
f e threshold = do state <- use Id.counter
                   (result, state') <- runStateT (runReaderT (g Map.empty e) threshold) state
                   assign Id.counter state'
                   pure result
