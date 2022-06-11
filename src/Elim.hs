module Elim where
import qualified Data.List as List
import qualified Data.Set as Set
import           KNormal (Exp (..), FunDef (..), fv)
import           Logging
import           MyPrelude

-- 副作用の有無
effect :: Exp -> Bool
effect (IfEq _ _ e1 e2) = effect e1 || effect e2
effect (IfLE _ _ e1 e2) = effect e1 || effect e2
effect (Let _ e1 e2)    = effect e1 || effect e2
effect (LetRec _ e)     = effect e
effect (LetTuple _ _ e) = effect e
effect (App _ _)        = True
effect (Put _ _ _)      = True
effect (ExtFunApp _ _)  = True
effect _                = False

f :: MonadLogger m => Exp -> m Exp
f (IfEq x y e1 e2) = IfEq x y <$> f e1 <*> f e2
f (IfLE x y e1 e2) = IfLE x y <$> f e1 <*> f e2
f (Let (x, t) e1 e2) = do
  e1' <- f e1
  e2' <- f e2
  if effect e1' || Set.member x (fv e2') then
    pure $ Let (x, t) e1' e2'
    else do
    putLogLn $ "eliminating variable " ++ x
    pure e2'
f (LetRec (FunDef { name = (x, t), args = yts, body = e1 }) e2) = do
  e2' <- f e2
  if Set.member x (fv e2') then
    do e1' <- f e1
       pure $ LetRec (FunDef { name = (x, t), args = yts, body = e1' }) e2'
    else do
    putLogLn $ "eliminating function " ++ x
    pure e2'
f (LetTuple xts y e) = do
  let xs = map fst xts
  e' <- f e
  let live = fv e'
  if List.any (\x -> Set.member x live) xs then
    pure $ LetTuple xts y e'
    else do
    putLogLn $ "eliminating variables " ++ List.concat (List.intersperse " " xs)
    pure e'
f e = pure e
