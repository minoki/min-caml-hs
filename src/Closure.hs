module Closure where
import           Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Id (Id)
import qualified Id
import qualified KNormal
import           Logging
import           MyPrelude
import qualified Type

data Closure = Closure { entry    :: Id.Label
                       , actualFv :: [Id]
                       }
             deriving Show

data Exp = Unit
         | Int Int
         | Float Double
         | Neg Id
         | Add Id Id
         | Sub Id Id
         | FNeg Id
         | FAdd Id Id
         | FSub Id Id
         | FMul Id Id
         | FDiv Id Id
         | IfEq Id Id Exp Exp
         | IfLE Id Id Exp Exp
         | Let (Id, Type.Type) Exp Exp
         | Var Id
         | MakeCls (Id, Type.Type) Closure Exp
         | AppCls Id [Id]
         | AppDir Id.Label [Id]
         | Tuple [Id]
         | LetTuple [(Id, Type.Type)] Id Exp
         | Get Id Id
         | Put Id Id Id
         | ExtArray Id.Label
         deriving Show

data FunDef = FunDef { name     :: (Id.Label, Type.Type)
                     , args     :: [(Id, Type.Type)]
                     , formalFv :: [(Id, Type.Type)]
                     , body     :: Exp
                     }
            deriving Show

data Prog = Prog [FunDef] Exp
          deriving Show

-- free variables
fv :: Exp -> Set.Set Id
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Float _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (FNeg x) = Set.singleton x
fv (FAdd x y) = Set.fromList [x, y]
fv (FSub x y) = Set.fromList [x, y]
fv (FMul x y) = Set.fromList [x, y]
fv (FDiv x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLE x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x, _) e1 e2) = Set.union (fv e1) (Set.delete x (fv e2))
fv (Var x) = Set.singleton x
fv (MakeCls (x, _) (Closure { entry = _, actualFv = ys }) e) = Set.delete x (Set.union (Set.fromList ys) (fv e))
fv (AppCls x ys) = Set.fromList (x : ys)
fv (AppDir _ xs) = Set.fromList xs
fv (Tuple xs) = Set.fromList xs
fv (LetTuple xts y e) = Set.insert y (Set.difference (fv e) (Set.fromList (map fst xts)))
fv (Get x y) = Set.fromList [x, y]
fv (Put x y z) = Set.fromList [x, y, z]
fv (ExtArray _) = Set.empty

type M m = StateT [FunDef] m -- toplevel

g :: MonadLogger m => Map.Map Id Type.Type -> Set.Set Id -> KNormal.Exp -> M m Exp
g _ _ KNormal.Unit = pure Unit
g _ _ (KNormal.Int i) = pure $ Int i
g _ _ (KNormal.Float d) = pure $ Float d
g _ _ (KNormal.Neg x) = pure $ Neg x
g _ _ (KNormal.Add x y) = pure $ Add x y
g _ _ (KNormal.Sub x y) = pure $ Sub x y
g _ _ (KNormal.FNeg x) = pure $ FNeg x
g _ _ (KNormal.FAdd x y) = pure $ FAdd x y
g _ _ (KNormal.FSub x y) = pure $ FSub x y
g _ _ (KNormal.FMul x y) = pure $ FMul x y
g _ _ (KNormal.FDiv x y) = pure $ FDiv x y
g env known (KNormal.IfEq x y e1 e2) = IfEq x y <$> g env known e1 <*> g env known e2
g env known (KNormal.IfLE x y e1 e2) = IfLE x y <$> g env known e1 <*> g env known e2
g env known (KNormal.Let (x, t) e1 e2) = Let (x, t) <$> g env known e1 <*> g (Map.insert x t env) known e2
g _ _ (KNormal.Var x) = pure $ Var x
g env known (KNormal.LetRec (KNormal.FunDef { KNormal.name = (x, t), KNormal.args = yts, KNormal.body = e1 }) e2)
  = do let env' = Map.insert x t env
       (known', e1') <- do
         -- ????????????let rec x y1 ... yn = e1 in e2???????????????
         -- x???????????????????????????????????????known???????????????e1????????????????????????????????????
         toplevel_backup <- get
         let known' = Set.insert x known
         e1' <- g (List.foldl' (\m (y, t) -> Map.insert y t m) env' yts) known' e1
         -- ??????????????????????????????????????????????????????e1'???????????????
         -- ?????????e1'???x?????????????????????????????????????????????closure????????????
         let zs = Set.difference (fv e1') (Set.fromList (map fst yts))
         if Set.null zs then
           pure (known', e1')
         else do
           -- ???????????????????????????????????????????????????????????????????????????
           putLogLn $ "free variable(s) " ++ List.concat (List.intersperse " " (Set.toList zs)) ++ " found in function " ++ x
           putLogLn $ "function " ++ x ++ " cannot be directly applied in fact"
           do put toplevel_backup
              e1'' <- g (List.foldl' (\m (y, t) -> Map.insert y t m) env' yts) known e1
              pure (known, e1'')
       let zs = Set.toList $ Set.difference (fv e1') (Set.insert x (Set.fromList (map fst yts))) -- ????????????????????????
       let zts = map (\z -> (z, env' Map.! z)) zs -- ?????????????????????z??????????????????????????????env?????????
       modify (FunDef { name = (Id.Label x, t), args = yts, formalFv = zts, body = e1' } :)
       e2' <- g env' known' e2
       if Set.member x (fv e2') then -- x??????????????????e2'??????????????????
         pure $ MakeCls (x, t) (Closure { entry = Id.Label x, actualFv = zs }) e2' -- ????????????????????????????????????
       else do
         putLogLn $ "eliminating closure(s) " ++ x
         pure e2' -- ?????????????????????MakeCls?????????
g _ known (KNormal.App x ys) | Set.member x known = do putLogLn $ "directly applying " ++ x
                                                       pure $ AppDir (Id.Label x) ys
                             | otherwise = pure $ AppCls x ys
g _ _ (KNormal.Tuple xs) = pure $ Tuple xs
g env known (KNormal.LetTuple xts y e) = LetTuple xts y <$> g (List.foldl' (\m (x, t) -> Map.insert x t m) env xts) known e
g _ _ (KNormal.Get x y) = pure $ Get x y
g _ _ (KNormal.Put x y z) = pure $ Put x y z
g _ _ (KNormal.ExtArray x) = pure $ ExtArray (Id.Label x)
g _ _ (KNormal.ExtFunApp x ys) = pure $ AppDir (Id.Label ("min_caml_" ++ x)) ys

f :: MonadLogger m => KNormal.Exp -> m Prog
f e = do (e', toplevel) <- runStateT (g Map.empty Set.empty e) []
         pure $ Prog (reverse toplevel) e'
