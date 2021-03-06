module Alpha where
import           Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Id (Id)
import qualified Id
import           KNormal (Exp (..), FunDef (..))
import           Lens.Micro.Mtl (assign, use)
import           MyPrelude

type M = State Id.Counter

find :: Id -> Map.Map Id Id -> Id
find x env = Map.findWithDefault x x env

-- Also used by Inline.g
g :: Map.Map Id Id -> Exp -> M Exp
g _env Unit = pure Unit
g _env (Int i) = pure (Int i)
g _env (Float d) = pure (Float d)
g env (Neg x) = pure (Neg (find x env))
g env (Add x y) = pure (Add (find x env) (find y env))
g env (Sub x y) = pure (Sub (find x env) (find y env))
g env (FNeg x) = pure (FNeg (find x env))
g env (FAdd x y) = pure (FAdd (find x env) (find y env))
g env (FSub x y) = pure (FSub (find x env) (find y env))
g env (FMul x y) = pure (FMul (find x env) (find y env))
g env (FDiv x y) = pure (FDiv (find x env) (find y env))
g env (IfEq x y e1 e2) = IfEq (find x env) (find y env) <$> g env e1 <*> g env e2
g env (IfLE x y e1 e2) = IfLE (find x env) (find y env) <$> g env e1 <*> g env e2
g env (Let (x, t) e1 e2) = do x' <- Id.genId x
                              Let (x', t) <$> g env e1 <*> g (Map.insert x x' env) e2
g env (Var x) = pure (Var (find x env))
g env (LetRec (FunDef { name = (x, t), args = yts, body = e1 }) e2)
  = do env' <- (\x' -> Map.insert x x' env) <$> Id.genId x
       let ys = map fst yts
       env'' <- (\ys' -> List.foldl' (\m (y, y') -> Map.insert y y' m) env' (zip ys ys')) <$> mapM Id.genId ys
       fundec <- FunDef (find x env', t) (map (\(y, t) -> (find y env'', t)) yts) <$> g env'' e1
       LetRec fundec <$> g env' e2
g env (App x ys) = pure (App (find x env) (map (`find` env) ys))
g env (Tuple xs) = pure (Tuple (map (`find` env) xs))
g env (LetTuple xts y e) = do
  let xs = map fst xts
  env' <- (\xs' -> List.foldl' (\m (x, x') -> Map.insert x x' m) env (zip xs xs')) <$> mapM Id.genId xs
  LetTuple (map (\(x, t) -> (find x env', t)) xts) (find y env) <$> g env' e
g env (Get x y) = pure (Get (find x env) (find y env))
g env (Put x y z) = pure (Put (find x env) (find y env) (find z env))
g _env (ExtArray x) = pure (ExtArray x)
g env (ExtFunApp x ys) = pure (ExtFunApp x (map (`find` env) ys))

f :: (MonadState s m, Id.HasCounter s) => Exp -> m Exp
f exp = do state <- use Id.counter
           let (result, state') = runState (g Map.empty exp) state
           assign Id.counter state'
           pure result
