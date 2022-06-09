module Typing where
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.STRef
import qualified Id
import           MyPrelude
import qualified Syntax as S
import qualified Type

type Env s = Map.Map Id.Id (Type.TypeF (STRef s))
type M s = StateT (Map.Map Id.Id (Type.TypeF (STRef s))) (ExceptT String (ST s))

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = do r <- a
             if r then pure True else b

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = pure False
anyM f (x:xs) = f x `orM` anyM f xs

occur :: STRef s (Maybe (Type.TypeF (STRef s))) -> Type.TypeF (STRef s) -> ST s Bool
occur r1 (Type.Fun t2s t2) = anyM (occur r1) t2s `orM` occur r1 t2
occur r1 (Type.Tuple t2s) = anyM (occur r1) t2s
occur r1 (Type.Array t2) = occur r1 t2
occur r1 (Type.Var r2) | r1 == r2 = pure True
                       | otherwise = do c2 <- readSTRef r2
                                        case c2 of
                                          Nothing -> pure False
                                          Just t2 -> occur r1 t2
occur _ _ = pure False

unify :: String -> Type.TypeF (STRef s) -> Type.TypeF (STRef s) -> M s ()
unify _ Type.Unit Type.Unit = pure ()
unify _ Type.Bool Type.Bool = pure ()
unify _ Type.Int Type.Int = pure ()
unify _ Type.Float Type.Float = pure ()
unify loc t1@(Type.Fun t1s t1') t2@(Type.Fun t2s t2') = do
  when (length t1s /= length t2s) $ do
    t1f <- lift $ lift $ freezeType t1
    t2f <- lift $ lift $ freezeType t2
    throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"
  zipWithM_ (unify loc) t1s t2s
  unify loc t1' t2'
unify loc t1@(Type.Tuple t1s) t2@(Type.Tuple t2s) = do
  when (length t1s /= length t2s) $ do
    t1f <- lift $ lift $ freezeType t1
    t2f <- lift $ lift $ freezeType t2
    throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"
  zipWithM_ (unify loc) t1s t2s
unify loc (Type.Array t1) (Type.Array t2) = unify loc t1 t2
unify _ (Type.Var r1) (Type.Var r2) | r1 == r2 = pure ()
unify loc (Type.Var r1) t2 = do
  c1 <- lift $ lift $ readSTRef r1
  case c1 of
    Just t1' -> unify loc t1' t2
    Nothing -> do o <- lift $ lift $ occur r1 t2
                  if o then
                    throwError $ "occur check (" ++ loc ++ ")"
                  else
                    lift $ lift $ writeSTRef r1 (Just t2)
unify loc t1 (Type.Var r2) = do
  c2 <- lift $ lift $ readSTRef r2
  case c2 of
    Just t2' -> unify loc t1 t2'
    Nothing -> do o <- lift $ lift $ occur r2 t1
                  if o then
                    throwError $ "occur check (" ++ loc ++ ")"
                  else
                    lift $ lift $ writeSTRef r2 (Just t1)
unify loc t1 t2 = do t1f <- lift $ lift $ freezeType t1
                     t2f <- lift $ lift $ freezeType t2
                     throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"


g :: Env s -> S.ExpF (STRef s) -> M s (Type.TypeF (STRef s))
g _env S.Unit = pure Type.Unit
g _env (S.Bool _) = pure Type.Bool
g _env (S.Int _) = pure Type.Int
g _env (S.Float _) = pure Type.Float
g env (S.Not e) = do e' <- g env e
                     unify "not" Type.Bool e'
                     pure Type.Bool
g env (S.Neg e) = do e' <- g env e
                     unify "unary -" Type.Int e'
                     pure Type.Int
g env (S.Add x y) = do x' <- g env x
                       unify "+" Type.Int x'
                       y' <- g env y
                       unify "+" Type.Int y'
                       pure Type.Int
g env (S.Sub x y) = do x' <- g env x
                       unify "-" Type.Int x'
                       y' <- g env y
                       unify "-" Type.Int y'
                       pure Type.Int
g env (S.FNeg e) = do e' <- g env e
                      unify "unary -." Type.Float e'
                      pure Type.Float
g env (S.FAdd x y) = do x' <- g env x
                        unify "+." Type.Float x'
                        y' <- g env y
                        unify "+." Type.Float y'
                        pure Type.Float
g env (S.FSub x y) = do x' <- g env x
                        unify "-." Type.Float x'
                        y' <- g env y
                        unify "-." Type.Float y'
                        pure Type.Float
g env (S.FMul x y) = do x' <- g env x
                        unify "*." Type.Float x'
                        y' <- g env y
                        unify "*." Type.Float y'
                        pure Type.Float
g env (S.FDiv x y) = do x' <- g env x
                        unify "/." Type.Float x'
                        y' <- g env y
                        unify "/." Type.Float y'
                        pure Type.Float
g env (S.Eq x y) = do x' <- g env x
                      y' <- g env y
                      unify "=" x' y'
                      pure Type.Bool
g env (S.LE x y) = do x' <- g env x
                      y' <- g env y
                      unify "<>" x' y'
                      pure Type.Bool
g env (S.If x y z) = do x' <- g env x
                        unify "if" x' Type.Bool
                        y' <- g env y
                        z' <- g env z
                        unify "if" y' z'
                        pure y'
g env (S.Let (x, t) e1 e2) = do t1 <- g env e1
                                unify "let" t t1
                                g (Map.insert x t env) e2
g env (S.Var x) | Just t <- Map.lookup x env = pure t
                | otherwise = do extenv <- get
                                 case Map.lookup x extenv of
                                   Just t -> pure t -- 外部変数
                                   Nothing -> do
                                     -- message: "free variable " ++ x ++ " assumed as external."
                                     t <- lift $ lift Type.genTyp
                                     modify (Map.insert x t)
                                     pure t
g env (S.LetRec (S.FunDef { S.name = (x, t), S.args = yts, S.body = e1 }) e2) = do
          let env' = Map.insert x t env
          resultTy <- g (List.foldl' (\m (y, u) -> Map.insert y u m) env' yts) e1
          unify "let rec" t (Type.Fun (List.map snd yts) resultTy)
          g env' e2
g env (S.App e es) = do resultTy <- lift $ lift Type.genTyp
                        funcTy <- g env e
                        argTypes <- mapM (g env) es
                        unify "app" funcTy (Type.Fun argTypes resultTy)
                        pure resultTy
g env (S.Tuple es) = Type.Tuple <$> mapM (g env) es
g env (S.LetTuple xts e1 e2) = do t1 <- g env e1
                                  unify "let tuple" (Type.Tuple (List.map snd xts)) t1
                                  g (List.foldl' (\m (x, t) -> Map.insert x t m) env xts) e2
g env (S.Array e1 e2) = do t1 <- g env e1
                           unify "array" t1 Type.Int
                           Type.Array <$> g env e2
g env (S.Get e1 e2) = do t <- lift $ lift Type.genTyp
                         t1 <- g env e1
                         unify "get" (Type.Array t) t1
                         t2 <- g env e2
                         unify "get" Type.Int t2
                         pure t
g env (S.Put e1 e2 e3) = do t <- g env e3
                            t1 <- g env e1
                            unify "put" (Type.Array t) t1
                            t2 <- g env e2
                            unify "put" Type.Int t2
                            pure Type.Unit

fillFreshType :: Type.TypeF Identity -> ST s (Type.TypeF (STRef s))
fillFreshType = Type.mapTypeM f
  where f (Identity Nothing) = Type.Var <$> newSTRef Nothing
        f (Identity (Just x)) = do x' <- fillFreshType x
                                   Type.Var <$> newSTRef (Just x')

fillFreshTypesInExp :: S.ExpF Identity -> ST s (S.ExpF (STRef s))
fillFreshTypesInExp = S.mapExpM fillFreshType

freezeType :: Type.TypeF (STRef s) -> ST s Type.Type
freezeType = Type.mapTypeM f
  where f r = do m <- readSTRef r
                 case m of
                   Nothing -> do
                     -- uninstantiated type variable detected; assuming int
                     writeSTRef r (Just Type.Int)
                     pure Type.Int
                   Just t -> freezeType t

freezeTypesInExp :: S.ExpF (STRef s) -> ST s S.Exp
freezeTypesInExp = S.mapExpM freezeType

f :: S.ExpF Identity -> Either String (S.Exp, Map.Map Id.Id Type.Type)
f e = runST $ do
  e' <- fillFreshTypesInExp e
  let initialEnv = Map.empty
      initialExtEnv = Map.empty
  result <- runExceptT (execStateT (g initialEnv e' >>= unify "toplevel" Type.Unit) initialExtEnv)
  case result of
    Left msg -> pure (Left msg)
    Right extenv -> do e'' <- freezeTypesInExp e'
                       extenv' <- traverse freezeType extenv
                       pure (Right (e'', extenv'))


