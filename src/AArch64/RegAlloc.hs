{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module AArch64.RegAlloc where
import           AArch64.Asm
import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Foldable (foldlM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Id (Id)
import qualified Id
import           Lens.Micro.Mtl (assign, use)
import           Logging
import           MyPrelude
import qualified Type

type M m = StateT Id.Counter (ExceptT String m)

-- for register coalescing
-- [XXX] Callがあったら、そこから先は無意味というか逆効果なので追わない。
--       そのために「Callがあったかどうか」を返り値の第1要素に含める。
target' :: Id -> (Id, Type.Type) -> Exp -> (Bool, [Id])
target' src (dest, t) e = case e of
  Mov x | x == src && is_reg dest -> assert (t /= Type.Unit && t /= Type.Float) (False, [dest])
  FMovD x | x == src && is_reg dest -> assert (t == Type.Float) (False, [dest])
  IfEq _ _ e1 e2 -> let (c1, rs1) = target src (dest, t) e1
                        (c2, rs2) = target src (dest, t) e2
                    in (c1 && c2, rs1 ++ rs2)
  IfLE _ _ e1 e2 -> let (c1, rs1) = target src (dest, t) e1
                        (c2, rs2) = target src (dest, t) e2
                    in (c1 && c2, rs1 ++ rs2)
  IfGE _ _ e1 e2 -> let (c1, rs1) = target src (dest, t) e1
                        (c2, rs2) = target src (dest, t) e2
                    in (c1 && c2, rs1 ++ rs2)
  IfFEq _ _ e1 e2 -> let (c1, rs1) = target src (dest, t) e1
                         (c2, rs2) = target src (dest, t) e2
                     in (c1 && c2, rs1 ++ rs2)
  IfFLE _ _ e1 e2 -> let (c1, rs1) = target src (dest, t) e1
                         (c2, rs2) = target src (dest, t) e2
                     in (c1 && c2, rs1 ++ rs2)
  CallCls _ ys zs -> (True, target_args src allregs ys ++ target_args src allfregs zs)
  _ -> (False, [])

-- register targeting
target :: Id -> (Id, Type.Type) -> Instructions -> (Bool, [Id])
target src dest (Ans exp) = target' src dest exp
target src dest (Let xt exp e) = case target' src xt exp of
                                   u@(True, _) -> u
                                   (False, rs1) -> let (c2, rs2) = target src dest e
                                                   in (c2, rs1 ++ rs2)

target_args :: Id -> [Id] -> [Id] -> [Id]
target_args _ _ [] = []
target_args src (r : rs) (y : ys) | src == y = r : target_args src rs ys
                                  | otherwise = target_args src rs ys
target_args _ [] _ = error "target_args"

-- allocにおいてspillingがあったかどうかを表すデータ型
data AllocResult = Alloc Id -- allocated register
                 | Spill Id -- spilled variable
alloc :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Id -> Type.Type -> m AllocResult
alloc dest cont regenv x t = assert (not (Map.member x regenv))
  $ let all = case t of
                Type.Unit  -> ["%x0"] -- dummy
                Type.Float -> allfregs
                _          -> allregs
    in if all == ["%x0"] then
         pure $ Alloc "%x0"
       else if is_reg x then
              pure $ Alloc x
            else
              let free = fv cont
                  (_, prefer) = target x dest cont
                  live = List.foldl' (\live y -> if is_reg y then
                                                   Set.insert y live
                                                 else
                                                   case Map.lookup y regenv of
                                                     Just y' -> Set.insert y' live
                                                     Nothing -> live) Set.empty free
              in case List.find (\r -> not (Set.member r live)) (prefer ++ all) of
                   Just r -> pure $ Alloc r
                   Nothing -> do
                     putLogLn $ "register allocation failed for " ++ x
                     let y = List.find (\y -> not (is_reg y) && case Map.lookup y regenv of
                                                                  Just y' -> y' `elem` all
                                                                  Nothing -> False) (List.reverse free)
                     case y of
                       Just y' -> do putLogLn $ "spilling " ++ y' ++ " from " ++ (regenv Map.! y')
                                     pure $ Spill y'
                       Nothing -> error "Spill"

add :: Id -> Id -> Map.Map Id Id -> Map.Map Id Id
add x r regenv | is_reg x = assert (x == r) regenv
               | otherwise = Map.insert x r regenv

-- Left: NoReg
find :: Id -> Type.Type -> Map.Map Id Id -> Either (Id, Type.Type) Id
find x t regenv | is_reg x = Right x
                | otherwise = case Map.lookup x regenv of
                                Just y  -> Right y
                                Nothing -> Left (x, t)

find' :: IdOrImm -> Map.Map Id Id -> Either (Id, Type.Type) IdOrImm
find' (V x) regenv = V <$> find x Type.Int regenv
find' c _          = Right c

g :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Instructions -> M m (Instructions, Map.Map Id Id)
g dest cont regenv (Ans exp) = g'_and_restore dest cont regenv exp
g dest cont regenv (Let xt@(x, t) exp e)
  = do let !_ = assert (not (Map.member x regenv)) ()
           cont' = concat e dest cont
       (e1', regenv1) <- g'_and_restore xt cont' regenv exp
       allocResult <- lift $ lift $ alloc dest cont' regenv1 x t
       case allocResult of
         Spill y -> do let r = regenv1 Map.! y
                       (e2', regenv2) <- g dest cont (add x r (Map.delete y regenv1)) e
                       let save = case Map.lookup y regenv of
                                    Just y' -> Save y' y
                                    Nothing -> Nop
                       (\s -> (s, regenv2)) <$> seq save (concat e1' (r, t) e2')
         Alloc r -> do (e2', regenv2) <- g dest cont (add x r regenv1) e
                       pure (concat e1' (r, t) e2', regenv2)

-- 使用される変数をスタックからレジスタへRestore
g'_and_restore :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> M m (Instructions, Map.Map Id Id)
g'_and_restore dest cont regenv exp = do r <- g' dest cont regenv exp
                                         case r of
                                           Right result -> pure result
                                           Left (x, t) -> g dest cont regenv (Let (x, t) (Restore x) (Ans exp))

-- 各命令のレジスタ割り当て
g' :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> M m (Either (Id, Type.Type) (Instructions, Map.Map Id Id))
g' _ _ regenv exp@Nop = pure $ Right (Ans exp, regenv)
g' _ _ regenv exp@(Set _) = pure $ Right (Ans exp, regenv)
g' _ _ regenv exp@(SetL _) = pure $ Right (Ans exp, regenv)
g' _ _ regenv exp@(Comment _) = pure $ Right (Ans exp, regenv)
g' _ _ regenv exp@(Restore _) = pure $ Right (Ans exp, regenv)
g' _ _ regenv (Mov x) = pure $ do e <- Mov <$> find x Type.Int regenv
                                  pure (Ans e, regenv)
g' _ _ regenv (Neg x) = pure $ do e <- Neg <$> find x Type.Int regenv
                                  pure (Ans e, regenv)
g' _ _ regenv (Add x y') = pure $ do e <- Add <$> find x Type.Int regenv <*> find' y' regenv
                                     pure (Ans e, regenv)
g' _ _ regenv (Sub x y') = pure $ do e <- Sub <$> find x Type.Int regenv <*> find' y' regenv
                                     pure (Ans e, regenv)
g' _ _ regenv (SLL x y') = pure $ do e <- SLL <$> find x Type.Int regenv <*> find' y' regenv
                                     pure (Ans e, regenv)
g' _ _ regenv (Ld x y') = pure $ do e <- Ld <$> find x Type.Int regenv <*> find' y' regenv
                                    pure (Ans e, regenv)
g' _ _ regenv (St x y z') = pure $ do e <- St <$> find x Type.Int regenv <*> find y Type.Int regenv <*> find' z' regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (FMovD x) = pure $ do e <- FMovD <$> find x Type.Float regenv
                                    pure (Ans e, regenv)
g' _ _ regenv (FNegD x) = pure $ do e <- FNegD <$> find x Type.Float regenv
                                    pure (Ans e, regenv)
g' _ _ regenv (FAddD x y) = pure $ do e <- FAddD <$> find x Type.Float regenv <*> find y Type.Float regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (FSubD x y) = pure $ do e <- FSubD <$> find x Type.Float regenv <*> find y Type.Float regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (FMulD x y) = pure $ do e <- FMulD <$> find x Type.Float regenv <*> find y Type.Float regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (FDivD x y) = pure $ do e <- FDivD <$> find x Type.Float regenv <*> find y Type.Float regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (LdDF x y') = pure $ do e <- LdDF <$> find x Type.Int regenv <*> find' y' regenv
                                      pure (Ans e, regenv)
g' _ _ regenv (StDF x y z') = pure $ do e <- StDF <$> find x Type.Float regenv <*> find y Type.Int regenv <*> find' z' regenv
                                        pure (Ans e, regenv)
g' dest cont regenv exp@(IfEq x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfEq <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfLE x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfLE <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfGE x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfGE <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfFEq x y e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfFEq <$> find x Type.Float regenv <*> find y Type.Float regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfFLE x y e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfFLE <$> find x Type.Float regenv <*> find y Type.Float regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(CallCls x ys zs) | length ys > length allregs - 2 || length zs > length allfregs - 1 = throwError $ "cannot allocate registers for arguments to " ++ x
                                          | otherwise = g'_call dest cont regenv exp (\ys zs -> CallCls <$> find x Type.Int regenv <*> pure ys <*> pure zs) ys zs
g' dest cont regenv exp@(CallDir l@(Id.Label x) ys zs) | length ys > length allregs - 1 || length zs > length allfregs - 1 = throwError $ "cannot allocate registers for arguments to " ++ x
                                                       | otherwise= g'_call dest cont regenv exp (\ys zs -> pure $ CallDir l ys zs) ys zs
g' _ _ _ (Save _ _) = error "unexpected Save"

-- ifのレジスタ割り当て
g'_if :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> (Instructions -> Instructions -> Either (Id, Type.Type) Exp) -> Instructions -> Instructions -> M m (Either (Id, Type.Type) (Instructions, Map.Map Id Id))
g'_if dest cont regenv _ constr e1 e2
  = do (e1', regenv1) <- g dest cont regenv e1
       (e2', regenv2) <- g dest cont regenv e2
           -- 両方に共通のレジスタ変数だけ利用
       let regenv' = List.foldl' (\regenv' x ->
                                    if is_reg x then
                                      regenv'
                                    else
                                      case (Map.lookup x regenv1, Map.lookup x regenv2) of
                                        (Just r1, Just r2) | r1 == r2 -> Map.insert x r1 regenv'
                                        _ -> regenv')
                     Map.empty
                     (fv cont)
       case constr e1' e2' of
         Left t -> pure $ Left t
         Right e0 -> do e <- foldlM (\e x ->
                                       if x == fst dest || not (Map.member x regenv) || Map.member x regenv' then
                                         pure e
                                       else
                                         seq (Save (regenv Map.! x) x) e) -- そうでない変数は分岐直前にセーブ
                             (Ans e0) (fv cont)
                        pure $ Right (e, regenv')

-- 関数呼び出しのレジスタ割り当て
g'_call :: MonadLogger m => (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> ([Id] -> [Id] -> Either (Id, Type.Type) Exp) -> [Id] -> [Id] -> M m (Either (Id, Type.Type) (Instructions, Map.Map Id Id))
g'_call dest cont regenv _ constr ys zs
  = do let m = do ys' <- mapM (\y -> find y Type.Int regenv) ys
                  zs' <- mapM (\z -> find z Type.Float regenv) zs
                  constr ys' zs'
       case m of
         Left t -> pure $ Left t
         Right e0 -> do e <- foldlM (\e x ->
                                        if x == fst dest || not (Map.member x regenv) then
                                          pure e
                                        else
                                          seq (Save (regenv Map.! x) x) e)
                             (Ans e0) (fv cont)
                        pure $ Right (e, Map.empty)

-- 関数のレジスタ割り当て
h :: MonadLogger m => FunDef -> M m FunDef
h (FunDef { name = Id.Label x, args = ys, fargs = zs, body = e, ret = t })
  = do let regenv = Map.singleton x reg_cl
           (arg_regs, regenv') = List.foldl' (\(arg_regs, regenv) (y, r) ->
                                                (arg_regs ++ [r], assert (not (is_reg y)) $ Map.insert y r regenv))
                                 ([], regenv) (zip ys allregs)
           (farg_regs, regenv'') = List.foldl' (\(farg_regs, regenv) (z, fr) ->
                                                  (farg_regs ++ [fr], assert (not (is_reg z)) $ Map.insert z fr regenv))
                                   ([], regenv') (zip zs allfregs)
       a <- case t of
              Type.Unit  -> Id.genTmp Type.Unit
              Type.Float -> pure $ head allfregs
              _          -> pure $ head allregs
       (e', _) <- g (a, t) (Ans (Mov a)) regenv'' e
       pure $ FunDef { name = Id.Label x, args = arg_regs, fargs = farg_regs, body = e', ret = t }

-- プログラム全体のレジスタ割り当て
f :: (MonadLogger m, MonadError String m, MonadState s m, Id.HasCounter s) => Prog -> m Prog
f (Prog dat fundefs e) = do state <- use Id.counter
                            result <- runExceptT $ flip runStateT state $ do putLogLn "register allocation: may take some time"
                                                                             fundefs' <- mapM h fundefs
                                                                             v <- Id.genTmp Type.Unit
                                                                             (e', _) <- g (v, Type.Unit) (Ans Nop) Map.empty e
                                                                             pure $ Prog dat fundefs' e'
                            case result of
                              Left e  -> throwError e
                              Right (p, state') -> do assign Id.counter state'
                                                      pure p
