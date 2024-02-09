module Lib.Elab (ElabCtx(..), check, infer) where

import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import GHC.Base
import GHC.Num
import Lib.Eval
import Lib.Meta
import Lib.Raw
import Lib.Syntax
import Lib.Unify
import Lib.Value
import Lib.Value.Env

-- | types of every variable in scope
type Bounds = [(Name, VTy)]

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
    metaCtx = lens _metaCtx (\ctx mc -> ctx{_metaCtx = mc})

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
    go i ((x', a) : tys)
        | x == x' = return (Var i, a)
        | otherwise = go (i + 1) tys

bind :: Monad m => Name -> VTy -> ElabM m a -> ElabM m a
bind x a = extendBound . locally bounds ((x, a):)

define :: Monad m => Name -> Val -> VTy -> ElabM m a -> ElabM m a
define x t a = extendDefined t . locally bounds ((x, a):)

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

unifyFun :: (MonadCatch m, MonadIO m) => Val -> ElabM m (Val, Clos)
unifyFun (VPi _ a c) = return (a, c)
unifyFun t = do
    a <- eval' =<< freshMeta
    env <- view envL
    c <- Clos env <$> freshMeta
    unifyCatch (VPi "x" a c) t
    return (a, c)

-- | Bidirectional algorithm
check :: (MonadCatch m, MonadIO m) => Raw -> VTy -> ElabM m Tm
check (RSrcPos pos t) a = locally srcPos (const pos) $ check t a
check (RLam x t) (VPi _ a c) =
    bind x a $ Lam x <$> (check t =<< instClos c)
check (RLet x a t u) b = do
    a' <- check a VU
    va <- eval' a'
    t' <- check t va
    vt <- eval' t'
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check RHole _ = freshMeta -- untyped unification: for PVWS, only kind *
check t a = do
    (t', a') <- infer t
    unifyCatch a a'
    return t'

infer :: (MonadCatch m, MonadIO m) => Raw -> ElabM m (Tm, VTy)
infer (RSrcPos pos t) = locally srcPos (const pos) $ infer t
infer (RVar x) = lookupBounds x
infer RU = return (U, VU)
infer (RApp t u) = do
    (t', a) <- infer t
    (a', c) <- unifyFun a
    u' <- check u a'
    (App t' u',) <$> ((c @) =<< eval' u')
infer RLam{} = throwString  "Can't infer type for lambda expression"
infer (RPi x a b) = do
    a' <- check a VU
    va <- eval' a'
    b' <- bind x va $ check b VU
    pure (Pi x a' b', VU)
infer (RLet x a t u) = do
    a' <- check a VU
    va <- eval' a'
    t' <- check t va
    vt <- eval' t'
    (u', uty) <- define x vt va $ infer u
    pure (Let x a' t' u', uty)
infer RHole = do
    a <- eval' =<< freshMeta
    t <- freshMeta
    return (t, a)
