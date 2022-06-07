{-# LANGUAGE BangPatterns #-}
module RegAlloc where
import Prelude hiding (concat, seq)
import qualified Id
import qualified Type
import AArch64Asm
import Id (Id)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Control.Exception (assert)

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
alloc :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Id -> Type.Type -> AllocResult
alloc dest cont regenv x t = assert (not (Map.member x regenv))
  $ let all = case t of
                Type.Unit -> ["%x0"] -- dummy
                Type.Float -> allfregs
                _ -> allregs
    in if all == ["%x0"] then
         Alloc "%x0"
       else if is_reg x then
              Alloc x
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
                   Just r -> Alloc r
                   Nothing -> -- message: register allocation failed for <x>
                     let y = List.find (\y -> not (is_reg y) && case Map.lookup y regenv of
                                                                  Just y' -> y' `elem` all
                                                                  Nothing -> False) (List.reverse free)
                     in -- message: "spilling <y> from <regenv Map.! y>
                        case y of
                          Just y' -> Spill y'
                          Nothing -> error "Spill"

add :: Id -> Id -> Map.Map Id Id -> Map.Map Id Id
add x r regenv | is_reg x = assert (x == r) regenv
               | otherwise = Map.insert x r regenv

-- Left: NoReg
find :: Id -> Type.Type -> Map.Map Id Id -> Either (Id, Type.Type) Id
find x t regenv | is_reg x = Right x
                | otherwise = case Map.lookup x regenv of
                                Just y -> Right y
                                Nothing -> Left (x, t)

find' :: IdOrImm -> Map.Map Id Id -> Either (Id, Type.Type) IdOrImm
find' (V x) regenv = V <$> find x Type.Int regenv
find' c _ = Right c

g :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Instructions -> (Instructions, Map.Map Id Id)
g dest cont regenv (Ans exp) = g'_and_restore dest cont regenv exp
g dest cont regenv (Let xt@(x, t) exp e)
  = let !_ = assert (not (Map.member x regenv)) ()
        cont' = concat e dest cont
        (e1', regenv1) = g'_and_restore xt cont' regenv exp
    in case alloc dest cont' regenv1 x t of
         Spill y -> let r = regenv1 Map.! y
                        (e2', regenv2) = g dest cont (add x r (Map.delete y regenv1)) e
                        save = case Map.lookup y regenv of
                                 Just y' -> Save y' y
                                 Nothing -> Nop
                    in (seq save (concat e1' (r, t) e2'), regenv2)
         Alloc r -> let (e2', regenv2) = g dest cont (add x r regenv1) e
                    in (concat e1' (r, t) e2', regenv2)

-- 使用される変数をスタックからレジスタへRestore
g'_and_restore :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> (Instructions, Map.Map Id Id)
g'_and_restore dest cont regenv exp = case g' dest cont regenv exp of
                                        Right result -> result
                                        Left (x, t) -> g dest cont regenv (Let (x, t) (Restore x) (Ans exp))

-- 各命令のレジスタ割り当て
g' :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> Either (Id, Type.Type) (Instructions, Map.Map Id Id)
g' _ _ regenv exp@Nop = pure (Ans exp, regenv)
g' _ _ regenv exp@(Set _) = pure (Ans exp, regenv)
g' _ _ regenv exp@(SetL _) = pure (Ans exp, regenv)
g' _ _ regenv exp@(Comment _) = pure (Ans exp, regenv)
g' _ _ regenv exp@(Restore _) = pure (Ans exp, regenv)
g' _ _ regenv (Mov x) = do e <- Mov <$> find x Type.Int regenv
                           pure (Ans e, regenv)
g' _ _ regenv (Neg x) = do e <- Neg <$> find x Type.Int regenv
                           pure (Ans e, regenv)
g' _ _ regenv (Add x y') = do e <- Add <$> find x Type.Int regenv <*> find' y' regenv
                              pure (Ans e, regenv)
g' _ _ regenv (Sub x y') = do e <- Sub <$> find x Type.Int regenv <*> find' y' regenv
                              pure (Ans e, regenv)
g' _ _ regenv (SLL x y') = do e <- SLL <$> find x Type.Int regenv <*> find' y' regenv
                              pure (Ans e, regenv)
g' _ _ regenv (Ld x y') = do e <- Ld <$> find x Type.Int regenv <*> find' y' regenv
                             pure (Ans e, regenv)
g' _ _ regenv (St x y z') = do e <- St <$> find x Type.Int regenv <*> find y Type.Int regenv <*> find' z' regenv
                               pure (Ans e, regenv)
g' _ _ regenv (FMovD x) = do e <- FMovD <$> find x Type.Float regenv
                             pure (Ans e, regenv)
g' _ _ regenv (FNegD x) = do e <- FNegD <$> find x Type.Float regenv
                             pure (Ans e, regenv)
g' _ _ regenv (FAddD x y) = do e <- FAddD <$> find x Type.Float regenv <*> find y Type.Float regenv
                               pure (Ans e, regenv)
g' _ _ regenv (FSubD x y) = do e <- FSubD <$> find x Type.Float regenv <*> find y Type.Float regenv
                               pure (Ans e, regenv)
g' _ _ regenv (FMulD x y) = do e <- FMulD <$> find x Type.Float regenv <*> find y Type.Float regenv
                               pure (Ans e, regenv)
g' _ _ regenv (FDivD x y) = do e <- FDivD <$> find x Type.Float regenv <*> find y Type.Float regenv
                               pure (Ans e, regenv)
g' _ _ regenv (LdDF x y') = do e <- LdDF <$> find x Type.Int regenv <*> find' y' regenv
                               pure (Ans e, regenv)
g' _ _ regenv (StDF x y z') = do e <- StDF <$> find x Type.Float regenv <*> find y Type.Int regenv <*> find' z' regenv
                                 pure (Ans e, regenv)
g' dest cont regenv exp@(IfEq x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfEq <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfLE x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfLE <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfGE x y' e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfGE <$> find x Type.Int regenv <*> find' y' regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfFEq x y e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfFEq <$> find x Type.Float regenv <*> find y Type.Float regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(IfFLE x y e1 e2) = g'_if dest cont regenv exp (\e1' e2' -> IfFLE <$> find x Type.Float regenv <*> find y Type.Float regenv <*> pure e1' <*> pure e2') e1 e2
g' dest cont regenv exp@(CallCls x ys zs) = g'_call dest cont regenv exp (\ys zs -> CallCls <$> find x Type.Int regenv <*> pure ys <*> pure zs) ys zs
g' dest cont regenv exp@(CallDir l ys zs) = g'_call dest cont regenv exp (\ys zs -> pure $ CallDir l ys zs) ys zs
g' _ _ _ (Save _ _) = error "unexpected Save"

-- ifのレジスタ割り当て
g'_if :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> (Instructions -> Instructions -> Either (Id, Type.Type) Exp) -> Instructions -> Instructions -> Either (Id, Type.Type) (Instructions, Map.Map Id Id)
g'_if dest cont regenv _ constr e1 e2
  = do let (e1', regenv1) = g dest cont regenv e1
           (e2', regenv2) = g dest cont regenv e2
           -- 両方に共通のレジスタ変数だけ利用
           regenv' = List.foldl' (\regenv' x ->
                                    if is_reg x then
                                      regenv'
                                    else
                                      case (Map.lookup x regenv1, Map.lookup x regenv2) of
                                        (Just r1, Just r2) | r1 == r2 -> Map.insert x r1 regenv'
                                        _ -> regenv')
                     Map.empty
                     (fv cont)
       e0 <- Ans <$> constr e1' e2'
       pure (List.foldl' (\e x ->
                            if x == fst dest || not (Map.member x regenv) || Map.member x regenv' then
                              e
                            else
                              seq (Save (regenv Map.! x) x) e) -- そうでない変数は分岐直前にセーブ
              e0 (fv cont), regenv')

-- 関数呼び出しのレジスタ割り当て
g'_call :: (Id, Type.Type) -> Instructions -> Map.Map Id Id -> Exp -> ([Id] -> [Id] -> Either (Id, Type.Type) Exp) -> [Id] -> [Id] -> Either (Id, Type.Type) (Instructions, Map.Map Id Id)
g'_call dest cont regenv _ constr ys zs
  = do ys' <- mapM (\y -> find y Type.Int regenv) ys
       zs' <- mapM (\z -> find z Type.Float regenv) zs
       e0 <- Ans <$> constr ys' zs'
       pure $ (List.foldl' (\e x ->
                              if x == fst dest || not (Map.member x regenv) then
                                e
                              else
                                seq (Save (regenv Map.! x) x) e)
               e0 (fv cont), Map.empty)

-- 関数のレジスタ割り当て
h :: FunDef -> FunDef
h (FunDef { name = Id.Label x, args = ys, fargs = zs, body = e, ret = t })
  = let regenv = Map.singleton x reg_cl
        (arg_regs, regenv') = List.foldl' (\(arg_regs, regenv) (y, r) ->
                                             (arg_regs ++ [r], assert (not (is_reg y)) $ Map.insert y r regenv))
                              ([], regenv) (zip ys allregs)
        (farg_regs, regenv'') = List.foldl' (\(farg_regs, regenv) (z, fr) ->
                                               (farg_regs ++ [fr], assert (not (is_reg z)) $ Map.insert z fr regenv))
                                ([], regenv') (zip zs allfregs)
        a = case t of
              Type.Unit -> "_" -- オリジナルではId.gentmp Type.Unitしている
              Type.Float -> head allfregs
              _ -> head allregs
        (e', _) = g (a, t) (Ans (Mov a)) regenv'' e
    in FunDef { name = Id.Label x, args = arg_regs, fargs = farg_regs, body = e', ret = t }

-- プログラム全体のレジスタ割り当て
f :: Prog -> Prog
f (Prog dat fundefs e) = let fundefs' = map h fundefs
                             (e', _) = g ("_", Type.Unit) (Ans Nop) Map.empty e
                         in Prog dat fundefs' e'
