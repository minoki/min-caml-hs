module Typing where
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Functor.Const
import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.STRef
import qualified Id
import           MyPrelude
import qualified Syntax as S
import qualified Type
import qualified Logging

type Env s = Map.Map Id.Id (Type.TypeF (STRef s))
type M = StateT (Map.Map Id.Id (Type.TypeF (STRef RealWorld))) (ExceptT String IO)

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = do r <- a
             if r then pure True else b

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = pure False
anyM f (x:xs) = f x `orM` anyM f xs

genTyp :: ST s (Type.TypeF (STRef s))
genTyp = do r <- newSTRef Nothing
            return (Type.Var r)

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

unify :: String -> Type.TypeF (STRef s) -> Type.TypeF (STRef s) -> ExceptT String (ST s) ()
unify _ Type.Unit Type.Unit = pure ()
unify _ Type.Bool Type.Bool = pure ()
unify _ Type.Int Type.Int = pure ()
unify _ Type.Float Type.Float = pure ()
unify loc t1@(Type.Fun t1s t1') t2@(Type.Fun t2s t2') = do
  when (length t1s /= length t2s) $ do
    t1f <- lift $ printableType t1
    t2f <- lift $ printableType t2
    throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"
  zipWithM_ (unify loc) t1s t2s
  unify loc t1' t2'
unify loc t1@(Type.Tuple t1s) t2@(Type.Tuple t2s) = do
  when (length t1s /= length t2s) $ do
    t1f <- lift $ printableType t1
    t2f <- lift $ printableType t2
    throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"
  zipWithM_ (unify loc) t1s t2s
unify loc (Type.Array t1) (Type.Array t2) = unify loc t1 t2
unify loc t1@(Type.Var r1) t2@(Type.Var r2)
  | r1 == r2 = pure ()
  | otherwise = do
      c1 <- lift $ readSTRef r1
      c2 <- lift $ readSTRef r2
      case (c1, c2) of
        (Just t1', Just t2') -> unify loc t1' t2'
        (Just t1', Nothing) -> unify loc t1' t2
        (Nothing, Just t2') -> unify loc t1 t2'
        (Nothing, Nothing) -> lift $ writeSTRef r1 (Just t2)
unify loc (Type.Var r1) t2 = do
  c1 <- lift $ readSTRef r1
  case c1 of
    Just t1' -> unify loc t1' t2
    Nothing -> do o <- lift $ occur r1 t2
                  if o then
                    do t2f <- lift $ printableType t2
                       throwError $ "occur check: " ++ show t2f ++ " (" ++ loc ++ ")"
                  else
                    lift $ writeSTRef r1 (Just t2)
unify loc t1 (Type.Var r2) = do
  c2 <- lift $ readSTRef r2
  case c2 of
    Just t2' -> unify loc t1 t2'
    Nothing -> do o <- lift $ occur r2 t1
                  if o then
                    do t1f <- lift $ printableType t1
                       throwError $ "occur check: " ++ show t1f ++ " (" ++ loc ++ ")"
                  else
                    lift $ writeSTRef r2 (Just t1)
unify loc t1 t2 = do t1f <- lift $ printableType t1
                     t2f <- lift $ printableType t2
                     throwError $ "unify: " ++ show t1f ++ ", " ++ show t2f ++ " (" ++ loc ++ ")"

unify' :: String -> Type.TypeF (STRef RealWorld) -> Type.TypeF (STRef RealWorld) -> M ()
unify' loc t1 t2 = lift $ ExceptT $ stToIO $ runExceptT (unify loc t1 t2)

g :: Env RealWorld -> S.ExpF (STRef RealWorld) -> M (Type.TypeF (STRef RealWorld))
g _env (S.Unit _) = pure Type.Unit
g _env (S.Bool _ _) = pure Type.Bool
g _env (S.Int _ _) = pure Type.Int
g _env (S.Float _ _) = pure Type.Float
g env (S.Not a e) = do e' <- g env e
                       unify' ("not at " ++ show a) Type.Bool e'
                       pure Type.Bool
g env (S.Neg a e) = do e' <- g env e
                       unify' ("unary - at " ++ show a) Type.Int e'
                       pure Type.Int
g env (S.Add a x y) = do x' <- g env x
                         unify' ("+ at " ++ show a) Type.Int x'
                         y' <- g env y
                         unify' ("+ at " ++ show a) Type.Int y'
                         pure Type.Int
g env (S.Sub a x y) = do x' <- g env x
                         unify' ("- at " ++ show a) Type.Int x'
                         y' <- g env y
                         unify' ("- at " ++ show a) Type.Int y'
                         pure Type.Int
g env (S.FNeg a e) = do e' <- g env e
                        unify' ("unary -. at " ++ show a) Type.Float e'
                        pure Type.Float
g env (S.FAdd a x y) = do x' <- g env x
                          unify' ("+. at " ++ show a) Type.Float x'
                          y' <- g env y
                          unify' ("+. at " ++ show a) Type.Float y'
                          pure Type.Float
g env (S.FSub a x y) = do x' <- g env x
                          unify' ("-. at " ++ show a) Type.Float x'
                          y' <- g env y
                          unify' ("-. at " ++ show a) Type.Float y'
                          pure Type.Float
g env (S.FMul a x y) = do x' <- g env x
                          unify' ("*. at " ++ show a) Type.Float x'
                          y' <- g env y
                          unify' ("*. at " ++ show a) Type.Float y'
                          pure Type.Float
g env (S.FDiv a x y) = do x' <- g env x
                          unify' ("/. at " ++ show a) Type.Float x'
                          y' <- g env y
                          unify' ("/. at " ++ show a) Type.Float y'
                          pure Type.Float
g env (S.Eq a x y) = do x' <- g env x
                        y' <- g env y
                        unify' ("= at " ++ show a) x' y'
                        pure Type.Bool
g env (S.LE a x y) = do x' <- g env x
                        y' <- g env y
                        unify' ("<= at " ++ show a) x' y'
                        pure Type.Bool
g env (S.If a x y z) = do x' <- g env x
                          unify' ("if at " ++ show a) x' Type.Bool
                          y' <- g env y
                          z' <- g env z
                          unify' ("if at " ++ show a) y' z'
                          pure y'
g env (S.Let a (x, t) e1 e2) = do t1 <- g env e1
                                  unify' ("let at " ++ show a) t t1
                                  g (Map.insert x t env) e2
g env (S.Var _ x) | Just t <- Map.lookup x env = pure t
                  | otherwise = do extenv <- get
                                   case Map.lookup x extenv of
                                     Just t -> pure t -- 外部変数
                                     Nothing -> do
                                       Logging.putLogLn $ "free variable " ++ x ++ " assumed as external"
                                       t <- lift $ lift $ stToIO genTyp
                                       modify (Map.insert x t)
                                       pure t
g env (S.LetRec _ (S.FunDef { S.name = (x, t), S.args = yts, S.body = e1 }) e2) = do
          let env' = Map.insert x t env
          resultTy <- g (List.foldl' (\m (y, u) -> Map.insert y u m) env' yts) e1
          unify' "let rec" t (Type.Fun (List.map snd yts) resultTy)
          g env' e2
g env (S.App a e es) = do resultTy <- lift $ lift $ stToIO genTyp
                          funcTy <- g env e
                          argTypes <- mapM (g env) es
                          unify' ("app at " ++ show a) funcTy (Type.Fun argTypes resultTy)
                          pure resultTy
g env (S.Tuple _ es) = Type.Tuple <$> mapM (g env) es
g env (S.LetTuple a xts e1 e2) = do t1 <- g env e1
                                    unify' ("let tuple at " ++ show a) (Type.Tuple (List.map snd xts)) t1
                                    g (List.foldl' (\m (x, t) -> Map.insert x t m) env xts) e2
g env (S.Array a e1 e2) = do t1 <- g env e1
                             unify' ("array at " ++ show a) t1 Type.Int
                             Type.Array <$> g env e2
g env (S.Get a e1 e2) = do t <- lift $ lift $ stToIO genTyp
                           t1 <- g env e1
                           unify' ("get at " ++ show a) (Type.Array t) t1
                           t2 <- g env e2
                           unify' ("get at " ++ show a) Type.Int t2
                           pure t
g env (S.Put a e1 e2 e3) = do t <- g env e3
                              t1 <- g env e1
                              unify' ("put at " ++ show a) (Type.Array t) t1
                              t2 <- g env e2
                              unify' ("put at " ++ show a) Type.Int t2
                              pure Type.Unit

fillFreshType :: Type.TypeF Identity -> ST s (Type.TypeF (STRef s))
fillFreshType = Type.mapTypeM f
  where f (Identity Nothing) = Type.Var <$> newSTRef Nothing
        f (Identity (Just x)) = do x' <- fillFreshType x
                                   Type.Var <$> newSTRef (Just x')

fillFreshTypesInExp :: S.ExpF Identity -> ST s (S.ExpF (STRef s))
fillFreshTypesInExp = S.mapExpM fillFreshType

freezeType :: Type.TypeF (STRef RealWorld) -> IO Type.Type
freezeType = Type.mapTypeM f
  where f r = do m <- stToIO $ readSTRef r
                 case m of
                   Nothing -> do
                     Logging.putLogLn "uninstantiated type variable detected; assuming int"
                     stToIO $ writeSTRef r (Just Type.Int)
                     pure Type.Int
                   Just t -> freezeType t

freezeTypesInExp :: S.ExpF (STRef RealWorld) -> IO S.Exp
freezeTypesInExp = S.mapExpM freezeType

printableType :: Type.TypeF (STRef s) -> ST s (Type.TypeF (Const String))
printableType = Type.mapTypeM f
  where f r = do m <- readSTRef r
                 case m of
                   Nothing -> pure $ Type.Var (Const "?")
                   Just t -> printableType t

f :: S.ExpF Identity -> IO (Either String (S.Exp, Map.Map Id.Id Type.Type))
f e = do
  e' <- stToIO $ fillFreshTypesInExp e
  let initialEnv = Map.empty
      initialExtEnv = Map.empty
  result <- runExceptT (execStateT (g initialEnv e' >>= unify' "toplevel" Type.Unit) initialExtEnv)
  case result of
    Left msg -> pure (Left msg)
    Right extenv -> do e'' <- freezeTypesInExp e'
                       extenv' <- traverse freezeType extenv
                       pure (Right (e'', extenv'))
