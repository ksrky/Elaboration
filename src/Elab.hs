{-# LANGUAGE TemplateHaskell #-}

module Elab (
    ElabCtx(..),
    check,
    infer
    ) where

import           Common
import           Control.Exception.Safe
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.IORef
import           Eval
import           Meta
import qualified Raw
import           Syntax
import           Unify
import           Value
import           Value.Env                as Env

-- type of every variable in scope
type Bounds = [(Name, VTy)]

-- | Defined or Bound
data Locals
    = Here
    | Define Locals Name Type Term
    | Bind Locals Name Type
    deriving (Eq, Show)

-- | Elaboration context.
data ElabCtx = ElabCtx {
    _env        :: Env,
    _bounds     :: Bounds,
    _locals     :: Locals,
    _pruning    :: Pruning,
    _nextMetaId :: IORef Int,
    _srcPos     :: Raw.SrcPos
    }

makeClassy ''ElabCtx

instance HasMetaCtx ElabCtx where
    nextMetaId_ = nextMetaId

type ElabM = ReaderT ElabCtx

lookupBounds :: MonadThrow m => Name -> ElabM m (Term, VTy)
lookupBounds x = go 0 =<< view bounds
  where
    go _ [] = throwString "variable out of scope"
    go i ((x', a) : tys)
        | x == x' = return (Var i, a)
        | otherwise = go (i + 1) tys

bind :: Monad m => Name -> VTy -> ElabM m a -> ElabM m a
bind x a =
    local (\ctx -> ctx
        & env %~ Env.increment
        & bounds %~ ((x, a) :))

define :: Monad m => Name -> Val -> VTy -> ElabM m a -> ElabM m a
define x t a =
    local (\ctx -> ctx
        & env %~ (`Env.append` t)
        & bounds %~ ((x, a) :))

closeTy :: Locals -> Type -> Type
closeTy lcls b = case lcls of
    Here               -> b
    Bind lcls' x a     -> closeTy lcls' (Pi x Expl a b)
    Define lcls' x a t -> closeTy lcls' (Let x a t b)

freshMeta :: MonadIO m => VTy -> ElabM m Term
freshMeta a = do
    lvl <- views env Env.level
    lcls <- view locals
    closed <- evalTerm Env.empty . closeTy lcls =<< quote lvl a
    mvar <- newMetaVar closed
    prun <- view pruning
    return $ AppPruning (Meta mvar) prun

unifyCatch :: (MonadCatch m, MonadIO m) => Val -> Val -> ElabM m ()
unifyCatch t t' = do
    l <- views env Env.level
    unify l t t'
        `catch` \(UnifyError msg) -> throwString msg

insert' :: MonadIO m => Term -> VTy -> ElabM m (Term, VTy)
insert' t va = force va >>= \case
        VPi _ Impl a b -> do
            m <- freshMeta a
            e <- view env
            mv <- evalTerm e m
            insert' (App t m Impl) =<< b |@ mv
        va' -> pure (t, va')

insert :: MonadIO m => Term -> VTy -> ElabM m (Term, VTy)
insert t@(Lam _ Impl _) va = return (t, va)
insert t                va = insert' t va

insertUntilName :: (MonadIO m, MonadThrow m) => Name -> Term -> VTy -> ElabM m (Term, VTy)
insertUntilName name t va = force va >>= \case
    va'@(VPi x Impl a b) -> do
        if x == name then return (t, va')
        else do
            m <- freshMeta a
            e <- view env
            mv <- evalTerm e m
            insertUntilName name (App t m Impl) =<< b |@ mv
    _ -> throwString "NoNamedImplicitArg name"

-- | Bidirectional algorithm
check :: (MonadCatch m, MonadIO m) => Raw.Raw -> VTy -> ElabM m Term
check (Raw.SrcPos pos t) a = locally srcPos (const pos) $ check t a
check (Raw.Lam x fi t) (VPi x' i' a c) | either (\y -> y == x' && i' == Impl) (== i') fi = do
    l <- views env Env.level
    bind x a $ Lam x i' <$> (check t =<< c |@ VVar l)
check t (VPi x Impl a b) = do
    l <- views env Env.level
    bind x a $ Lam x Impl <$> (check t =<< b |@ VVar l)
check (Raw.Let x a t u) b = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    t' <- check t va
    vt <- (`evalTerm` t') =<< view env
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check Raw.Hole a = freshMeta a
check t a = do
    (t', tty) <- uncurry insert =<< infer t
    unifyCatch a tty
    return t'

infer :: (MonadCatch m, MonadIO m) => Raw.Raw -> ElabM m (Term, VTy)
infer (Raw.SrcPos pos t) = locally srcPos (const pos) $ infer t
infer (Raw.Var x) = lookupBounds x
infer Raw.U = return (U, VU)
infer Raw.Lam{} = throwString "Can't infer type for lambda expression"
infer (Raw.App t u fi) = do
    (i', t', tty) <- case fi of
        Left name -> do
            (t', tty) <- uncurry (insertUntilName name) =<< infer t
            pure (Impl, t', tty)
        Right Impl -> do
            (t', tty) <- infer t
            pure (Impl, t', tty)
        Right Expl -> do
            (t', tty) <- uncurry insert' =<< infer t
            pure (Expl, t', tty)
    (a, b) <- force tty >>= \case
        VPi _ i'' a b -> do
            unless (i' == i'') $ throwString "IcitMismatch i i'"
            pure (a, b)
        tty' -> do
            e <- view env
            a <- evalTerm e =<< freshMeta VU
            b <- bind "x" a $ Closure e <$> freshMeta VU
            unifyCatch tty' (VPi "x" i' a b)
            pure (a, b)
    u' <- check u a
    e <- view env
    cl <- (b |@) =<< evalTerm e u'
    pure (App t' u' i', cl)
infer (Raw.Pi x i a b) = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    b' <- bind x va $ check b VU
    return (Pi x i a' b', VU)
infer (Raw.Let x a t u) = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    t' <- check t va
    vt <- (`evalTerm` t') =<< view env
    (u', uty) <- define x vt va $ infer u
    return (Let x a' t' u', uty)
infer Raw.Hole = do
    a <- freshMeta VU >>= \m -> view env >>= (`evalTerm` m)
    t <- freshMeta a
    pure (t, a)
