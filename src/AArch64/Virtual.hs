module AArch64.Virtual where
import           AArch64.Asm
import qualified Closure
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable (foldlM)
import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Id
import           Lens.Micro (lens)
import           MyPrelude
import qualified Type

-- 状態：
-- * 識別子生成のためのカウンター
-- * 浮動小数点数の定数テーブル
data S = S !Id.Counter [(Id.Label, Double)]
type M = StateT S (Either String)

instance Id.HasCounter S where
  counter = lens (\(S c _) -> c) (\(S _ t) c -> S c t)

sameFloat :: Double -> Double -> Bool
sameFloat x y = x == y && isNegativeZero x == isNegativeZero y

addList :: Ord k => [(k, a)] -> Map.Map k a -> Map.Map k a
addList xts m = List.foldl' (\m (y, t) -> Map.insert y t m) m xts

classify :: Monad m => [(id, Type.Type)] -> acc -> {- float -} (acc -> id -> m acc) -> {- integer -} (acc -> id -> Type.Type -> m acc) -> m acc
classify xts ini addf addi = foldlM (\acc (x, t) -> case t of
                                                      Type.Unit  -> pure acc
                                                      Type.Float -> addf acc x
                                                      _          -> addi acc x t) ini xts

separate :: [(id, Type.Type)] -> ({- integers -} [id], {- floats -} [id])
separate xts = runIdentity $ classify
               xts
               ([], [])
               (\(int, float) x -> Identity (int, float ++ [x]))
               (\(int, float) x _ -> Identity (int ++ [x], float))

expand :: Monad m => [(id, Type.Type)] -> (Int, acc) -> {- float -} (id -> Int -> acc -> m acc) -> {- integer -} (id -> Type.Type -> Int -> acc -> m acc) -> m (Int, acc)
expand xts ini addf addi = classify
                           xts
                           ini
                           (\(offset, acc) x -> (,) (offset + 8) <$> addf x offset acc) -- offset is always 8-byte aligned
                           (\(offset, acc) x t -> (,) (offset + 8) <$> addi x t offset acc) -- integer/pointer size: 8 bytes

g :: Map.Map Id.Id Type.Type -> Closure.Exp -> M Instructions
g _ Closure.Unit = pure $ Ans Nop
g _ (Closure.Int i) = pure $ Ans $ Set i
g _ (Closure.Float d) = do S _ table <- get
                           l <- case List.find (\(_, d') -> sameFloat d d') table of
                             Just (l, _) -> pure l
                             Nothing -> do l <- Id.Label <$> Id.genId "l"
                                           modify (\(S counter table) -> S counter ((l, d) : table))
                                           pure l
                           x <- Id.genId "l"
                           pure $ Let (x, Type.Int) (SetL l) (Ans (LdDF x (C 0)))
g _ (Closure.Neg x) = pure $ Ans $ Neg x
g _ (Closure.Add x y) = pure $ Ans $ Add x (V y)
g _ (Closure.Sub x y) = pure $ Ans $ Sub x (V y)
g _ (Closure.FNeg x) = pure $ Ans $ FNegD x
g _ (Closure.FAdd x y) = pure $ Ans $ FAddD x y
g _ (Closure.FSub x y) = pure $ Ans $ FSubD x y
g _ (Closure.FMul x y) = pure $ Ans $ FMulD x y
g _ (Closure.FDiv x y) = pure $ Ans $ FDivD x y
g env (Closure.IfEq x y e1 e2) = Ans <$> case env Map.! x of
                                           Type.Bool -> IfEq x (V y) <$> g env e1 <*> g env e2
                                           Type.Int -> IfEq x (V y) <$> g env e1 <*> g env e2
                                           Type.Float -> IfFEq x y <$> g env e1 <*> g env e2
                                           _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.IfLE x y e1 e2) = Ans <$> case env Map.! x of
                                           Type.Bool -> IfLE x (V y) <$> g env e1 <*> g env e2
                                           Type.Int -> IfLE x (V y) <$> g env e1 <*> g env e2
                                           Type.Float -> IfFLE x y <$> g env e1 <*> g env e2
                                           _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.Let (x, t1) e1 e2) = do e1' <- g env e1
                                       e2' <- g (Map.insert x t1 env) e2
                                       pure $ AArch64.Asm.concat e1' (x, t1) e2'
g env (Closure.Var x) = case env Map.! x of
                          Type.Unit  -> pure $ Ans Nop
                          Type.Float -> pure $ Ans $ FMovD x
                          _          -> pure $ Ans $ Mov x
g env (Closure.MakeCls (x, t) (Closure.Closure { Closure.entry = l, Closure.actualFv = ys }) e2)
  = do e2' <- g (Map.insert x t env) e2
       (offset, store_fv) <- expand (map (\y -> (y, env Map.! y)) ys)
                             (8, e2')
                             (\y offset store_fv -> seq (StDF y x (C offset)) store_fv)
                             (\y _ offset store_fv -> seq (St y x (C offset)) store_fv)
       z <- Id.genId "l"
       Let (x, t) (Mov reg_hp)
         . Let (reg_hp, Type.Int) (Add reg_hp (C offset))
         . Let (z, Type.Int) (SetL l)
         <$> seq (St z x (C 0)) store_fv
g env (Closure.AppCls x ys) = do let (int, float) = separate (map (\y -> (y, env Map.! y)) ys)
                                 pure $ Ans $ CallCls x int float
g env (Closure.AppDir (Id.Label x) ys) = do let (int, float) = separate (map (\y -> (y, env Map.! y)) ys)
                                            pure $ Ans $ CallDir (Id.Label x) int float
g env (Closure.Tuple xs) = do y <- Id.genId "t"
                              (offset, store) <- expand (map (\x -> (x, env Map.! x)) xs)
                                                 (0, Ans (Mov y))
                                                 (\x offset store -> seq (StDF x y (C offset)) store)
                                                 (\x _ offset store -> seq (St x y (C offset)) store)
                              pure $ Let (y, Type.Tuple (map (env Map.!) xs)) (Mov reg_hp)
                                $ Let (reg_hp, Type.Int) (Add reg_hp (C offset))
                                store
g env (Closure.LetTuple xts y e2) = do
  let s = Closure.fv e2
  e2' <- g (List.foldl (\m (x, t) -> Map.insert x t m) env xts) e2
  let (_offset, load) = runIdentity $ expand
                        xts
                        (0, e2')
                        (\x offset load -> Identity $ if not (Set.member x s) then
                                                        load
                                                      else
                                                        Let (x, Type.Float) (LdDF y (C offset)) load)
                        (\x t offset load -> Identity $ if not (Set.member x s) then
                                                          load
                                                        else
                                                          Let (x, t) (Ld y (C offset)) load)
  pure load
g env (Closure.Get x y) = do offset <- Id.genId "o"
                             pure $ case env Map.! x of
                                      Type.Array Type.Unit -> Ans Nop
                                      Type.Array Type.Float -> Let (offset, Type.Int) (SLL y (C 3))
                                                               $ Ans (LdDF x (V offset))
                                      Type.Array _ -> Let (offset, Type.Int) (SLL y (C 3))
                                                      $ Ans (Ld x (V offset))
                                      _ -> error "Get applied to non-array"
g env (Closure.Put x y z) = do offset <- Id.genId "o"
                               pure $ case env Map.! x of
                                        Type.Array Type.Unit -> Ans Nop
                                        Type.Array Type.Float -> Let (offset, Type.Int) (SLL y (C 3))
                                                                 $ Ans (StDF z x (V offset))
                                        Type.Array _ -> Let (offset, Type.Int) (SLL y (C 3))
                                                        $ Ans (St z x (V offset))
                                        _ -> error "Put applied to non-array"
g _ (Closure.ExtArray (Id.Label x)) = pure $ Ans $ SetL $ Id.Label $ "min_caml_" ++ x

h :: Closure.FunDef -> M FunDef
h (Closure.FunDef { Closure.name = (Id.Label x, t), Closure.args = yts, Closure.formalFv = zts, Closure.body = e })
  = do let (int, float) = separate yts
       e' <- g (Map.insert x t (addList yts (addList zts Map.empty))) e
       let (_offset, load) = runIdentity $ expand
                             zts
                             (8, e')
                             (\z offset load -> Identity $ Let (z, Type.Float) (LdDF reg_cl (C offset)) load)
                             (\z t offset load -> Identity $ Let (z, t) (LdDF reg_cl (C offset)) load)
       pure $ case t of
                Type.Fun _ t2 -> FunDef { name = Id.Label x, args = int, fargs = float, body = load, ret = t2 }
                _ -> error "invalid function type"

f :: Closure.Prog -> Id.Counter -> Either String (Prog, Id.Counter)
f (Closure.Prog fundefs e) state = flip evalStateT (S state []) $ do
  fundefs' <- mapM h fundefs
  e' <- g Map.empty e
  S state' table <- get
  pure (Prog table fundefs' e', state')
