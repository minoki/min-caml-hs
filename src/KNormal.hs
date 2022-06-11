{-# LANGUAGE FlexibleContexts #-}
module KNormal where
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Id (Id)
import qualified Id
import           Lens.Micro.Mtl (assign, use)
import           MyPrelude
import qualified Syntax
import qualified Type

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
         | LetRec FunDef Exp
         | App Id [Id]
         | Tuple [Id]
         | LetTuple [(Id, Type.Type)] Id Exp
         | Get Id Id
         | Put Id Id Id
         | ExtArray Id
         | ExtFunApp Id [Id]
         deriving (Eq, Show)

data FunDef = FunDef { name :: (Id, Type.Type)
                     , args :: [(Id, Type.Type)]
                     , body :: Exp
                     }
            deriving (Eq, Show)

-- free variables
fv :: Exp -> Set.Set Id
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Float _) = Set.empty
fv (ExtArray _) = Set.empty
fv (Neg x) = Set.singleton x
fv (FNeg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (FAdd x y) = Set.fromList [x, y]
fv (FSub x y) = Set.fromList [x, y]
fv (FMul x y) = Set.fromList [x, y]
fv (FDiv x y) = Set.fromList [x, y]
fv (Get x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = Set.insert x (Set.insert y (Set.union (fv e1) (fv e2)))
fv (IfLE x y e1 e2) = Set.insert x (Set.insert y (Set.union (fv e1) (fv e2)))
fv (Let (x, _) e1 e2) = Set.union (fv e1) (Set.delete x (fv e2))
fv (Var x) = Set.singleton x
fv (LetRec (FunDef { name = (x, _), args = yts, body = e1 }) e2)
  = let zs = Set.difference (fv e1) (Set.fromList (map fst yts))
    in Set.delete x (Set.union zs (fv e2))
fv (App x ys) = Set.fromList (x : ys)
fv (Tuple xs) = Set.fromList xs
fv (ExtFunApp _ xs) = Set.fromList xs
fv (Put x y z) = Set.fromList [x, y, z]
fv (LetTuple xs y e) = Set.insert y (Set.difference (fv e) (Set.fromList (map fst xs)))

type M = ReaderT (Map.Map Id Type.Type) (StateT Id.Counter (Either String))

insertLet :: M (Exp, Type.Type) -> (Id -> M (Exp, r)) -> M (Exp, r)
insertLet m k = do (e, t) <- m
                   case e of
                     Var x -> k x
                     _ -> do x <- Id.genTmp t
                             (e', t') <- k x
                             pure (Let (x, t) e e', t')

g :: Map.Map Id Type.Type -> Syntax.Exp -> M (Exp, Type.Type)
g _ (Syntax.Unit _) = pure (Unit, Type.Unit)
g _ (Syntax.Bool _ b) = pure (Int (if b then 1 else 0), Type.Int)
g _ (Syntax.Int _ i) = pure (Int i, Type.Int)
g _ (Syntax.Float _ d) = pure (Float d, Type.Float)
g env (Syntax.Not a e) = g env (Syntax.If a e (Syntax.Bool a False) (Syntax.Bool a True))
g env (Syntax.Neg _ e) = insertLet (g env e)
                         $ \x -> pure (Neg x, Type.Int)
g env (Syntax.Add _ e1 e2) = insertLet (g env e1)
                             $ \x -> insertLet (g env e2)
                                     $ \y -> pure (Add x y, Type.Int)
g env (Syntax.Sub _ e1 e2) = insertLet (g env e1)
                             $ \x -> insertLet (g env e2)
                                     $ \y -> pure (Sub x y, Type.Int)
g env (Syntax.FNeg _ e) = insertLet (g env e)
                          $ \x -> pure (FNeg x, Type.Float)
g env (Syntax.FAdd _ e1 e2) = insertLet (g env e1)
                              $ \x -> insertLet (g env e2)
                                      $ \y -> pure (FAdd x y, Type.Float)
g env (Syntax.FSub _ e1 e2) = insertLet (g env e1)
                              $ \x -> insertLet (g env e2)
                                      $ \y -> pure (FSub x y, Type.Float)
g env (Syntax.FMul _ e1 e2) = insertLet (g env e1)
                              $ \x -> insertLet (g env e2)
                                      $ \y -> pure (FMul x y, Type.Float)
g env (Syntax.FDiv _ e1 e2) = insertLet (g env e1)
                              $ \x -> insertLet (g env e2)
                                      $ \y -> pure (FDiv x y, Type.Float)
g env cmp@(Syntax.Eq a _ _) = g env (Syntax.If a cmp (Syntax.Bool a True) (Syntax.Bool a False))
g env cmp@(Syntax.LE a _ _) = g env (Syntax.If a cmp (Syntax.Bool a True) (Syntax.Bool a False))
g env (Syntax.If a (Syntax.Not _ e1) e2 e3) = g env (Syntax.If a e1 e3 e2) -- notによる分岐を変換
g env (Syntax.If _ (Syntax.Eq _ e1 e2) e3 e4) = insertLet (g env e1)
                                                $ \x -> insertLet (g env e2)
                                                        $ \y -> do (e3', t3) <- g env e3
                                                                   (e4', _t4) <- g env e4
                                                                   pure (IfEq x y e3' e4', t3)
g env (Syntax.If _ (Syntax.LE _ e1 e2) e3 e4) = insertLet (g env e1)
                                                $ \x -> insertLet (g env e2)
                                                        $ \y -> do (e3', t3) <- g env e3
                                                                   (e4', _t4) <- g env e4
                                                                   pure (IfLE x y e3' e4', t3)
g env (Syntax.If a e1 e2 e3) = g env (Syntax.If a (Syntax.Eq a e1 (Syntax.Bool a False)) e3 e2) -- 比較のない分岐を変換
g env (Syntax.Let _ (x, t) e1 e2) = do (e1', _t1) <- g env e1
                                       (e2', t2) <- g (Map.insert x t env) e2
                                       pure (Let (x, t) e1' e2', t2)
g env (Syntax.Var _ x) | Just t <- Map.lookup x env = pure (Var x, t)
                       | otherwise = do
                           -- 外部配列の参照
                           extenv <- ask
                           case extenv Map.! x of
                             t@(Type.Array _) -> pure (ExtArray x, t)
                             _ -> throwError $ "external variable " ++ x ++ " does not have an array type"
g env (Syntax.LetRec _ (Syntax.FunDef { Syntax.name = (x, t), Syntax.args = yts, Syntax.body = e1 }) e2)
  = do let env' = Map.insert x t env
       (e2', t2) <- g env' e2
       (e1', _t1) <- g (List.foldl' (\m (y, t) -> Map.insert y t m) env' yts) e1
       pure (LetRec (FunDef { name = (x, t), args = yts, body = e1' }) e2', t2)
g env (Syntax.App _ (Syntax.Var _ f) e2s) | not (Map.member f env) = do
                                              extenv <- ask
                                              case extenv Map.! f of
                                                Type.Fun _ t -> let bind xs [] = pure (ExtFunApp f xs, t)
                                                                    bind xs (e2 : e2s) = insertLet (g env e2)
                                                                                         $ \x -> bind (xs ++ [x]) e2s
                                                                in bind [] e2s
                                                _ -> error "invalid external function"
g env (Syntax.App _ e1 e2s) = do g_e1@(_, u) <- g env e1
                                 case u of
                                   Type.Fun _ t -> insertLet (pure g_e1)
                                                   $ \f -> let bind xs [] = pure (App f xs, t)
                                                               bind xs (e2 : e2s) = insertLet (g env e2)
                                                                                    $ \x -> bind (xs ++ [x]) e2s
                                                           in bind [] e2s
                                   _ -> error "invalid external function"
g env (Syntax.Tuple _ es) = let bind xs ts [] = pure (Tuple xs, Type.Tuple ts)
                                bind xs ts (e : es) = do g_e@(_, t) <- g env e
                                                         insertLet (pure g_e)
                                                           $ \x -> bind (xs ++ [x]) (ts ++ [t]) es
                            in bind [] [] es
g env (Syntax.LetTuple _ xts e1 e2) = insertLet (g env e1)
                                      $ \y -> do (e2', t2) <- g (List.foldl' (\m (x, t) -> Map.insert x t m) env xts) e2
                                                 pure (LetTuple xts y e2', t2)
g env (Syntax.Array _ e1 e2) = insertLet (g env e1)
                               $ \x -> do g_e2@(_, t2) <- g env e2
                                          insertLet (pure g_e2)
                                            $ \y -> let l = case t2 of
                                                              Type.Float -> "create_float_array"
                                                              _ -> "create_array"
                                                    in pure (ExtFunApp l [x, y], Type.Array t2)
g env (Syntax.Get _ e1 e2) = do g_e1@(_, u) <- g env e1
                                case u of
                                  Type.Array t -> insertLet (pure g_e1)
                                                  $ \x -> insertLet (g env e2)
                                                          $ \y -> pure (Get x y, t)
                                  _ -> error "invalid array access"
g env (Syntax.Put _ e1 e2 e3) = insertLet (g env e1)
                                $ \x -> insertLet (g env e2)
                                        $ \y -> insertLet (g env e3)
                                                $ \z -> pure (Put x y z, Type.Unit)

f :: (MonadError String m, MonadState s m, Id.HasCounter s) => Syntax.Exp -> Map.Map Id Type.Type -> m Exp
f e extenv = do state <- use Id.counter
                case runStateT (runReaderT (g Map.empty e) extenv) state of
                  Left e -> throwError e
                  Right ((result, _), state') -> do
                    assign Id.counter state'
                    pure result
