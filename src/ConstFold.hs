module ConstFold where
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Id (Id)
import           KNormal (Exp (..), FunDef (..))
import           MyPrelude

g :: Map.Map Id Exp -> Exp -> Exp
g env (Var x) | Just (Int x') <- Map.lookup x env = Int x'
g env (Neg x) | Just (Int x') <- Map.lookup x env = Int (- x')
g env (Add x y) | Just (Int x') <- Map.lookup x env, Just (Int y') <- Map.lookup y env = Int (x' + y')
g env (Sub x y) | Just (Int x') <- Map.lookup x env, Just (Int y') <- Map.lookup y env = Int (x' - y')
g env (FNeg x) | Just (Float x') <- Map.lookup x env = Float (- x')
g env (FAdd x y) | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = Float (x' + y')
g env (FSub x y) | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = Float (x' - y')
g env (FMul x y) | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = Float (x' * y')
g env (FDiv x y) | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = Float (x' / y')
g env (IfEq x y e1 e2)
  | Just (Int x') <- Map.lookup x env, Just (Int y') <- Map.lookup y env = if x' == y' then g env e1 else g env e2
  | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = if x' == y' then g env e1 else g env e2
  | otherwise = IfEq x y (g env e1) (g env e2)
g env (IfLE x y e1 e2)
  | Just (Int x') <- Map.lookup x env, Just (Int y') <- Map.lookup y env = if x' <= y' then g env e1 else g env e2
  | Just (Float x') <- Map.lookup x env, Just (Float y') <- Map.lookup y env = if x' <= y' then g env e1 else g env e2
  | otherwise = IfLE x y (g env e1) (g env e2)
g env (Let (x, t) e1 e2) = let e1' = g env e1
                               e2' = g (Map.insert x e1' env) e2
                           in Let (x, t) e1' e2'
g env (LetRec (FunDef { name = x, args = ys, body = e1 }) e2)
  = LetRec (FunDef { name = x, args = ys, body = g env e1 }) (g env e2)
g env (LetTuple xts y e)
  | Just (Tuple ys) <- Map.lookup y env = List.foldl' (\e' (xt, z) -> Let xt (Var z) e') (g env e) (zip xts ys)
  | otherwise = LetTuple xts y (g env e)
g _ e = e

f :: Exp -> Exp
f = g Map.empty
