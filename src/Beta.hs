module Beta where
import qualified Data.Map.Strict as Map
import           Id (Id)
import           KNormal (Exp (..), FunDef (..))
import           Logging
import           MyPrelude

find :: Id -> Map.Map Id Id -> Id
find x env = Map.findWithDefault x x env

g :: MonadLogger m => Map.Map Id Id -> Exp -> m Exp
g _ e@Unit = pure e
g _ e@(Int _) = pure e
g _ e@(Float _) = pure e
g env (Neg x) = pure $ Neg (find x env)
g env (Add x y) = pure $ Add (find x env) (find y env)
g env (Sub x y) = pure $ Sub (find x env) (find y env)
g env (FNeg x) = pure $ FNeg (find x env)
g env (FAdd x y) = pure $ FAdd (find x env) (find y env)
g env (FSub x y) = pure $ FSub (find x env) (find y env)
g env (FMul x y) = pure $ FMul (find x env) (find y env)
g env (FDiv x y) = pure $ FDiv (find x env) (find y env)
g env (IfEq x y e1 e2) = IfEq (find x env) (find y env) <$> g env e1 <*> g env e2
g env (IfLE x y e1 e2) = IfLE (find x env) (find y env) <$> g env e1 <*> g env e2
g env (Let (x, t) e1 e2) -- letのβ簡約
  = do e1' <- g env e1
       case e1' of
         Var y -> do
           putLogLn $ "beta-reducing " ++ x ++ " = " ++ y
           g (Map.insert x y env) e2
         _ -> Let (x, t) e1' <$> g env e2
g env (LetRec (FunDef { name = xt, args = yts, body = e1 }) e2)
  = do e1' <- g env e1
       LetRec (FunDef { name = xt, args = yts, body = e1' }) <$> g env e2
g env (Var x) = pure $ Var (find x env)
g env (Tuple xs) = pure $ Tuple (map (\x -> find x env) xs)
g env (LetTuple xts y e) = LetTuple xts (find y env) <$> g env e
g env (Get x y) = pure $ Get (find x env) (find y env)
g env (Put x y z) = pure $ Put (find x env) (find y env) (find z env)
g env (App g xs) = pure $ App (find g env) (map (\x -> find x env) xs)
g _ e@(ExtArray _) = pure e
g env (ExtFunApp x ys) = pure $ ExtFunApp x (map (\y -> find y env) ys)

f :: MonadLogger m => Exp -> m Exp
f = g Map.empty
