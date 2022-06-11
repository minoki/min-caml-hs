module AArch64.Simm where
import           AArch64.Asm
import           Control.Monad
import           Data.Bits
import           Data.Functor.Identity
import qualified Data.Map.Strict as Map
import           Id (Id)
import           MyPrelude

type M = Identity

-- 命令列の即値最適化
g :: Map.Map Id Int -> Instructions -> M Instructions
g env (Ans exp) = Ans <$> g' env exp
g env (Let (x, t) (Set i) e) = do
  -- found immediate x = i
  e' <- g (Map.insert x i env) e
  if x `elem` fv e' then
    pure $ Let (x, t) (Set i) e'
    else
    -- erased redundant Set to x
    pure e'
g env (Let xt (SLL y (C i)) e) | Just y' <- Map.lookup y env = do
                                   -- for array access
                                   -- erased redundant SLL on x
                                   g env (Let xt (Set (y' `shiftL` i)) e)
g env (Let xt exp e) = Let xt <$> g' env exp <*> g env e

-- 各命令の即値最適化
g' :: Map.Map Id Int -> Exp -> M Exp
g' env (Add x (V y)) | Just y' <- Map.lookup y env, 0 <= y', y' <= 4095 = pure $ Add x (C y')
                     | Just y' <- Map.lookup y env, -4095 <= y', y' <= 0 = pure $ Sub x (C (- y'))
                     | Just x' <- Map.lookup x env, 0 <= x', x' <= 4095 = pure $ Add y (C x')
                     | Just x' <- Map.lookup x env, -4095 <= x', x' <= 0 = pure $ Sub y (C (- x'))
g' env (Sub x (V y)) | Just y' <- Map.lookup y env, 0 <= y', y' <= 4095 = pure $ Sub x (C y')
                     | Just y' <- Map.lookup y env, -4095 <= y', y' <= 0 = pure $ Add x (C (- y'))
g' env (SLL x (V y)) | Just y' <- Map.lookup y env, 0 <= y', y' <= 63 = pure $ SLL x (C y')
g' env (Ld x (V y)) | Just y' <- Map.lookup y env, 0 <= y', y' <= 32760 = pure $ Ld x (C y')
g' env (St x y (V z)) | Just z' <- Map.lookup z env, 0 <= z', z' <= 32760 = pure $ St x y (C z')
g' env (LdDF x (V y)) | Just y' <- Map.lookup y env, 0 <= y', y' <= 32760 = pure $ LdDF x (C y')
g' env (StDF x y (V z)) | Just z' <- Map.lookup z env, 0 <= z', z' <= 32760 = pure $ StDF x y (C z')
g' env (IfEq x (V y) e1 e2) | Just y' <- Map.lookup y env, 0 <= y', y' <= 4095 = IfEq x (C y') <$> g env e1 <*> g env e2
                            | Just x' <- Map.lookup x env, 0 <= x', x' <= 4095 = IfEq y (C x') <$> g env e1 <*> g env e2
g' env (IfLE x (V y) e1 e2) | Just y' <- Map.lookup y env, 0 <= y', y' <= 4095 = IfLE x (C y') <$> g env e1 <*> g env e2
                            | Just x' <- Map.lookup x env, 0 <= x', x' <= 4095 = IfGE y (C x') <$> g env e1 <*> g env e2
g' env (IfGE x (V y) e1 e2) | Just y' <- Map.lookup y env, 0 <= y', y' <= 4095 = IfGE x (C y') <$> g env e1 <*> g env e2
                            | Just x' <- Map.lookup x env, 0 <= x', x' <= 4095 = IfLE y (C x') <$> g env e1 <*> g env e2
g' env (IfEq x y' e1 e2) = IfEq x y' <$> g env e1 <*> g env e2
g' env (IfLE x y' e1 e2) = IfLE x y' <$> g env e1 <*> g env e2
g' env (IfGE x y' e1 e2) = IfGE x y' <$> g env e1 <*> g env e2
g' env (IfFEq x y' e1 e2) = IfFEq x y' <$> g env e1 <*> g env e2
g' env (IfFLE x y' e1 e2) = IfFLE x y' <$> g env e1 <*> g env e2
g' _ e = pure e

-- トップレベル関数の即値最適化
h :: FunDef -> M FunDef
h (FunDef { name = l, args = xs, fargs = ys, body = e, ret = t })
  = (\e' -> FunDef { name = l, args = xs, fargs = ys, body = e', ret = t }) <$> g Map.empty e

-- プログラム全体の即値最適化
f :: Prog -> M Prog
f (Prog dat fundefs e) = Prog dat <$> mapM h fundefs <*> g Map.empty e
