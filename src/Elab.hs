{-# LANGUAGE TemplateHaskell #-}

module Elab (ElabCtx(..), check) where

import           Common
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Eval
import           Norm
import qualified Raw
import           Syntax
import           Value
import           Value.Env                as Env

-- type of every variable in scope
type Bounds = [(Name, VTy)]

-- | Elaboration context.
data ElabCtx = ElabCtx {_env :: Env, _bounds :: Bounds, _srcPos :: Int}

makeClassy ''ElabCtx

type ElabM = ReaderT ElabCtx

lookupBounds :: MonadFail m => Name -> ElabM m (Term, VTy)
lookupBounds x = go 0 =<< view bounds
  where
    go _ [] = fail "variable out of scope"
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
    -- locally valEnv (VE.cons t) . locally types ((x, a):)

-- | Bidirectional algorithm
check :: MonadFail m => Raw.Raw -> VTy -> ElabM m Term
check (Raw.SrcPos pos t) a = locally srcPos (const pos) $ check t a
check (Raw.Lam x t) (VPi _ a c) = do
    l <- views env Env.level
    bind x a $ Lam x <$> check t (c |@ VVar l)
check (Raw.Let x a t u) b = do
    a' <- check a VU
    va <- views env (`eval` a')
    t' <- check t va
    vt <- views env (`eval` t')
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check t a = do
    (t', tty) <- infer t
    l <- views env Env.level
    conv l tty a
    return t'

infer :: MonadFail m => Raw.Raw -> ElabM m (Term, VTy)
infer (Raw.SrcPos pos t) = locally srcPos (const pos) $ infer t
infer (Raw.Var x) = lookupBounds x
infer Raw.U = return (U, VU)
infer (Raw.App t u) = do
    (t', tty) <- infer t
    case tty of
        VPi _ a c -> do
            u' <- check u a
            vu <- views env (`eval` u')
            return (App t' u', c |@ vu)
        _ -> fail "Expected a function type, instead inferred"
infer Raw.Lam{} = fail "Can't infer type for lambda expression"
infer (Raw.Pi x a b) = do
    a' <- check a VU
    va <- views env (`eval` a')
    b' <- bind x va $ check b VU
    return (Pi x a' b', VU)
infer (Raw.Let x a t u) = do
    a' <- check a VU
    va <- views env (`eval` a')
    t' <- check t va
    vt <- views env (`eval` t')
    (u', uty) <- define x vt va $ infer u
    return (Let x a' t' u', uty)
