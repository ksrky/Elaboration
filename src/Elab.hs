{-# LANGUAGE TemplateHaskell #-}

module Elab
    ( ElabCtx(..)
    , initElabCtx
    , runElabM
    , check
    , infer
    ) where

import Common
import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Eval
import Meta
import Raw qualified
import Syntax
import Unify
import Value
import Value.Env                as Env

-- type of every variable in scope
type Bounds = [(Name, ValTy)]

-- | Defined or Bound
data Local
    = Define Name Type Term
    | Bind Name Type
    deriving (Eq, Show)

-- | Elaboration context.
data ElabCtx = ElabCtx
    { _env        :: Env
    , _bounds     :: Bounds
    , _locals     :: [Local]
    , _pruning    :: Pruning
    , _nextMetaId :: IORef Int
    , _srcPos     :: Raw.SrcPos
    }

makeClassy ''ElabCtx

instance HasEnv ElabCtx where
    env_ = env

instance HasMetaCtx ElabCtx where
    nextMetaId_ = nextMetaId

initElabCtx :: MonadIO m => m ElabCtx
initElabCtx = do
    ref <- liftIO $ newIORef 0
    return $ ElabCtx
        { _env = Env.empty
        , _bounds = []
        , _locals = []
        , _pruning = []
        , _nextMetaId = ref
        , _srcPos = (0, 0)
        }

type ElabM = ReaderT ElabCtx

runElabM :: ElabM m a -> ElabCtx -> m a
runElabM = runReaderT

lookupBounds :: MonadThrow m => Name -> ElabM m (Term, ValTy)
lookupBounds x = go 0 =<< view bounds
  where
    go _ [] = throwString "variable out of scope"
    go i ((x', a) : tys)
        | x == x' = return (Var i, a)
        | otherwise = go (i + 1) tys

bind :: MonadIO m => Name -> ValTy -> ElabM m a -> ElabM m a
bind x va k = do
    a <- quote' va
    local (\ctx -> ctx
        & env %~ Env.increment
        & bounds %~ ((x, va) :)
        & locals %~ (Bind x a :)
        & pruning %~ (|> Nothing)
        ) k

define :: MonadIO m => Name -> Term -> ValTy -> ElabM m a -> ElabM m a
define x t va k = do
    vt <- evalTerm' t
    a <- quote' va
    local (\ctx -> ctx
        & env %~ (`Env.append` vt)
        & bounds %~ ((x, va) :)
        & locals %~ (Define x a t :)
        & pruning %~ (|> Just Expl)
        ) k

closeVal :: MonadIO m => Val -> ElabM m Closure
closeVal t = do
    l <- views env level
    mkClosure =<< quote (l + 1) t

closeTy :: [Local] -> Type -> Type
closeTy [] b                    = b
closeTy (Bind x a : lcls) b     = closeTy lcls (Pi x Expl a b)
closeTy (Define x a t : lcls) b = closeTy lcls (Let x a t b)

freshMeta :: MonadIO m => ValTy -> ElabM m Term
freshMeta a = do
    lcls <- view locals
    closed <- evalClosedTerm . closeTy lcls =<< quote' a
    mvar <- newMetaVar closed
    prun <- view pruning
    return $ AppPruning (Meta mvar) prun

unifyCatch :: (MonadCatch m, MonadIO m) => Val -> Val -> ElabM m ()
unifyCatch t t' = do
    l <- views env Env.level
    unify l t t'
        `catch` \(UnifyError msg) -> throwString msg

insert' :: MonadIO m => Term -> ValTy -> ElabM m (Term, ValTy)
insert' t va = force va >>= \case
    VPi _ Impl a b -> do
        m <- freshMeta a
        mv <- evalTerm' m
        insert' (App t m Impl) =<< b |@ mv
    va' -> return (t, va')

insert :: MonadIO m => Term -> ValTy -> ElabM m (Term, ValTy)
insert t@(Lam _ Impl _) va = return (t, va)
insert t                va = insert' t va

insertUntilName :: (MonadIO m, MonadThrow m) => Name -> Term -> ValTy -> ElabM m (Term, ValTy)
insertUntilName name t va = force va >>= \case
    va'@(VPi x Impl a b) -> do
        if x == name then return (t, va')
        else do
            m <- freshMeta a
            mv <- evalTerm' m
            insertUntilName name (App t m Impl) =<< b |@ mv
    _ -> throwString "NoNamedImplicitArg name"

-- | Bidirectional algorithm
check :: (MonadCatch m, MonadIO m) => Raw.Raw -> ValTy -> ElabM m Term
check (Raw.SrcPos pos t) a = locally srcPos (const pos) $ check t a
check (Raw.Lam x fi t) (VPi x' i' a c) | either (\y -> y == x' && i' == Impl) (== i') fi = do
    l <- views env Env.level
    bind x a $ Lam x i' <$> (check t =<< c |@ VVar l)
check t (VPi x Impl a b) = do
    l <- views env Env.level
    bind x a $ Lam x Impl <$> (check t =<< b |@ VVar l)
check (Raw.Let x a t u) b = do
    a' <- check a VU
    va <- evalTerm' a'
    t' <- check t va
    u' <- define x t' va $ check u b
    return $ Let x a' t' u'
check Raw.Hole a = freshMeta a
check t a = do
    (t', tty) <- uncurry insert =<< infer t
    unifyCatch a tty
    return t'

infer :: (MonadCatch m, MonadIO m) => Raw.Raw -> ElabM m (Term, ValTy)
infer (Raw.SrcPos pos t) = locally srcPos (const pos) $ infer t
infer (Raw.Var x) = lookupBounds x
infer Raw.U = return (U, VU)
infer (Raw.Lam x (Right i) t) = do
    a <- evalTerm' =<< freshMeta VU
    (t', b) <- bind x a $ uncurry insert =<< infer t
    b' <- closeVal b
    return (Lam x i t', VPi x i a b')
infer Raw.Lam{} = throwString "Can't infer type for named lambda"
infer (Raw.App t u fi) = do
    (i', t', tty) <- case fi of
        Left name -> do
            (t', tty) <- uncurry (insertUntilName name) =<< infer t
            return (Impl, t', tty)
        Right Impl -> do
            (t', tty) <- infer t
            return (Impl, t', tty)
        Right Expl -> do
            (t', tty) <- uncurry insert' =<< infer t
            return (Expl, t', tty)
    (a, b) <- force tty >>= \case
        VPi _ i'' a b -> do
            unless (i' == i'') $ throwString "IcitMismatch i i'"
            return (a, b)
        tty' -> do
            a <- evalTerm' =<< freshMeta VU
            b <- bind "x" a $ mkClosure =<< freshMeta VU
            unifyCatch tty' (VPi "x" i' a b)
            return (a, b)
    u' <- check u a
    cl <- (b |@) =<< evalTerm' u'
    return (App t' u' i', cl)
infer (Raw.Pi x i a b) = do
    a' <- check a VU
    va <- evalTerm' a'
    b' <- bind x va $ check b VU
    return (Pi x i a' b', VU)
infer (Raw.Let x a t u) = do
    a' <- check a VU
    va <- evalTerm' a'
    t' <- check t va
    (u', uty) <- define x t' va $ infer u
    return (Let x a' t' u', uty)
infer Raw.Hole = do
    a <- freshMeta VU >>= \m -> view env >>= (`evalTerm` m)
    t <- freshMeta a
    return (t, a)
