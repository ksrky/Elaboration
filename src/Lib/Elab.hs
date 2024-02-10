module Lib.Elab (ElabCtx(..), check, infer) where

import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import GHC.Num
import GHC.Show
import Lib.Common
import Lib.Eval
import Lib.Meta
import Lib.Raw
import Lib.Syntax
import Lib.Unify
import Lib.Value
import Lib.Value.Env

data Origin = Inserted | Source
    deriving (Eq, Show)

-- | types of every variable in scope
type Bounds = [(Name, Origin, VTy)]

-- | Elaboration context.
data ElabCtx = ElabCtx {
    _env     :: Env,
    _metaCtx :: MetaCtx,
    _bounds  :: Bounds,
    _srcPos  :: SrcPos
    }

-- ** Lenses

instance HasEnv ElabCtx where
    envL = lens _env (\ctx env -> ctx{_env = env})

instance HasMetaCtx ElabCtx where
    metaCtxL = lens _metaCtx (\ctx mc -> ctx{_metaCtx = mc})

bounds :: Lens' ElabCtx Bounds
bounds = lens _bounds (\ctx bs -> ctx{_bounds = bs})

srcPos :: Lens' ElabCtx SrcPos
srcPos = lens _srcPos (\ctx pos -> ctx{_srcPos = pos})

-- | Elaboration monad
type ElabM = ReaderT ElabCtx

lookupBounds :: MonadThrow m => Name -> ElabM m (Tm, VTy)
lookupBounds x = go 0 =<< view bounds
  where
    go _ [] = throwString  "variable out of scope"
    go i ((x', _, a) : tys)
        | x == x' = return (Var i, a)
        | otherwise = go (i + 1) tys

bind :: Monad m => Name -> VTy -> ElabM m a -> ElabM m a
bind x a = extendBound . locally bounds ((x, Source, a):)

ibind :: Monad m => Name -> VTy -> ElabM m a -> ElabM m a
ibind x a = extendBound . locally bounds ((x, Inserted, a):)

define :: Monad m => Name -> Val -> VTy -> ElabM m a -> ElabM m a
define x t a = extendDefined t . locally bounds ((x, Source, a):)

freshMeta :: (MonadReader r m, HasMetaCtx r, MonadIO m) => m Tm
freshMeta = do
    m <- newMVar
    return $ IMeta m []

eval' :: (MonadThrow m, MonadIO m) => Tm -> ElabM m Val
eval' t = do
    env <- view envL
    eval env t

unifyCatch :: (MonadCatch m, MonadIO m) => Val -> Val -> ElabM m ()
unifyCatch t t' = unify t t'
    `catch` \(UnifyError msg) -> throwString msg

-- | Insert fresh implicit application
insert' :: (MonadThrow m, MonadIO m) => (Tm, VTy) -> ElabM m (Tm, VTy)
insert' (t, va) = do
    force va >>= \case
        VPi _ Impl _ c -> do
            m <- freshMeta
            mv <- eval' m
            insert' =<< (App t m Impl,) <$> c @ mv
        _ -> throwString "impossible"

insert :: (MonadThrow m, MonadIO m) => (Tm, VTy) -> ElabM m (Tm, VTy)
insert (t@(Lam _ Impl _), va) = return (t, va)
insert (t, va)                = insert' (t, va)

insertUntilName :: (MonadThrow m, MonadIO m) => Name -> (Tm, VTy) -> ElabM m (Tm, VTy)
insertUntilName x (t, va) = force va >>= \case
    VPi x' Impl _ c
        | x == x' -> return (t, va)
        | otherwise -> do
            m <- freshMeta
            mv <- eval' m
            insertUntilName x =<< (App t m Impl,) <$> c @ mv
    _ -> throwString "impossible"


unifyFun :: (MonadCatch m, MonadIO m) => Val -> Icit -> ElabM m (Val, Clos)
unifyFun (VPi _ i' a c) i = do
    unless (i == i') $ throwString "Icit mismatch"
    return (a, c)
unifyFun t i = do
    a <- eval' =<< freshMeta
    env <- view envL
    c <- Clos env <$> freshMeta
    unifyCatch (VPi "x" i a c) t
    return (a, c)

-- | Bidirectional algorithm
check :: (MonadCatch m, MonadIO m) => Raw -> VTy -> ElabM m Tm
check (RSrcPos pos t) a = locally srcPos (const pos) $ check t a
check (RLam x fi t) (VPi x' i a c)
    -- TODO: VPi should also have ArgInfo instead of Icit
    | either (\y -> y == x' && i == Impl) (== i) fi  =
    bind x a $ Lam x i <$> (check t =<< instClos c)
check t (VPi x Impl a c) =
    ibind x a $ Lam x Impl <$> (check t =<< instClos c)
check (RLet x a t u) b = do
    a' <- check a VU
    va <- eval' a'
    t' <- check t va
    vt <- eval' t'
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check RHole _ = freshMeta -- untyped unification: only kind * in PVWS
check t a = do
    (t', a') <- insert =<< infer t
    unifyCatch a a'
    return t'

infer :: (MonadCatch m, MonadIO m) => Raw -> ElabM m (Tm, VTy)
infer (RSrcPos pos t) = locally srcPos (const pos) $ infer t
infer (RVar x) = lookupBounds x
infer (RApp t u fi) = do
    (i, t', a) <- case fi of
        Left x -> do
            (t', a) <- insertUntilName x =<< infer t
            return (Impl, t', a)
        Right Expl -> do
            (t', a) <- insert' =<< infer t
            return (Expl, t', a)
        Right Impl -> do
            (t', a) <- infer t
            return (Impl, t', a)
    (a', c) <- unifyFun a i
    u' <- check u a'
    (App t' u' i,) <$> ((c @) =<< eval' u')
infer (RLam x (Right i) t) = do
    a <- eval' =<< freshMeta
    (t', b) <- bind x a $ insert =<< infer t
    env <- view envL
    c <- closeVal env b
    return (Lam x i t', VPi x i a c)
infer RLam{} = throwString  "Can't infer type for lambda expression"
infer RU = return (U, VU)
infer (RPi x i a b) = do
    a' <- check a VU
    va <- eval' a'
    b' <- bind x va $ check b VU
    return (Pi x i a' b', VU)
infer (RLet x a t u) = do
    a' <- check a VU
    va <- eval' a'
    t' <- check t va
    vt <- eval' t'
    (u', uty) <- define x vt va $ infer u
    return (Let x a' t' u', uty)
infer RHole = do
    a <- eval' =<< freshMeta
    t <- freshMeta
    return (t, a)
