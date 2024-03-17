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

-- | Elaboration context.
data ElabCtx = ElabCtx {
    _env        :: Env,
    _envSpecs   :: [EnvSpec],
    _bounds     :: Bounds,
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

freshMeta :: MonadIO m => ElabM m Term
freshMeta = do
    mvar <- newMetaVar
    especs <- view envSpecs
    return $ InsertedMeta mvar especs

unifyCatch :: (MonadCatch m, MonadIO m) => Val -> Val -> ElabM m ()
unifyCatch t t' = do
    l <- views env Env.level
    unify l t t'
        `catch` \(UnifyError msg) -> throwString msg

-- | Bidirectional algorithm
check :: (MonadCatch m, MonadIO m) => Raw.Raw -> VTy -> ElabM m Term
check (Raw.SrcPos pos t) a = locally srcPos (const pos) $ check t a
check (Raw.Lam x t) (VPi _ a c) = do
    l <- views env Env.level
    bind x a $ Lam x <$> (check t =<< c |@ VVar l)
check (Raw.Let x a t u) b = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    t' <- check t va
    vt <- (`evalTerm` t') =<< view env
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check Raw.Hole _ = freshMeta
check t a = do
    (t', tty) <- infer t
    unifyCatch a tty
    return t'

infer :: (MonadCatch m, MonadIO m) => Raw.Raw -> ElabM m (Term, VTy)
infer (Raw.SrcPos pos t) = locally srcPos (const pos) $ infer t
infer (Raw.Var x) = lookupBounds x
infer Raw.U = return (U, VU)
infer Raw.Lam{} = throwString "Can't infer type for lambda expression"
infer (Raw.App t u) = do
    (t', tty) <- infer t
    case tty of
        VPi _ a c -> do
            u' <- check u a
            vu <- (`evalTerm` u') =<< view env
            (App t' u',) <$> c |@ vu
        _ -> throwString "Expected a function type, instead inferred"
infer (Raw.Pi x a b) = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    b' <- bind x va $ check b VU
    return (Pi x a' b', VU)
infer (Raw.Let x a t u) = do
    a' <- check a VU
    va <- (`evalTerm` a') =<< view env
    t' <- check t va
    vt <- (`evalTerm` t') =<< view env
    (u', uty) <- define x vt va $ infer u
    return (Let x a' t' u', uty)
infer Raw.Hole = do
    a <- freshMeta >>= \m -> view env >>= (`evalTerm` m)
    t <- freshMeta
    pure (t, a)
