module Eval
    ( evalTerm
    , evalClosedTerm
    , evalTerm'
    , (|@)
    , mkClosure
    , vApp
    , force
    , lvl2Ix
    , quote
    , quote'
    , nf
    ) where

import Common
import Control.Lens.Combinators
import Control.Lens.Cons
import Control.Lens.Operators
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Meta
import Syntax
import Value
import Value.Env                  qualified as Env

-- | Evaluation
evalTerm :: MonadIO m => Env -> Term -> m Val
evalTerm env = \case
    Var i -> return $ Env.lookup i env
    App t u icit -> do
        t' <- evalTerm env t
        u' <- evalTerm env u
        vApp t' u' icit
    AppPruning t pr -> do
        t' <- evalTerm env t
        vAppPruning env t' pr
    Lam x icit t -> return $ VLam x icit (Closure env t)
    Let _ _ t u -> do
        t' <- evalTerm env t
        evalTerm (env |> t') u
    Pi x icit a b -> do
        a' <- evalTerm env a
        return $ VPi x icit a' (Closure env b)
    U -> return VU
    Meta mvar -> vMeta mvar

evalClosedTerm :: MonadIO m => Term -> m Val
evalClosedTerm = evalTerm Env.empty

evalTerm' :: (MonadReader r m, HasEnv r, MonadIO m) => Term -> m Val
evalTerm' t = do
    env <- view env_
    evalTerm env t

infixl 8 |@

-- | Closure application
(|@) :: MonadIO m => Closure -> Val -> m Val
Closure env t |@ v = evalTerm (Env.append env v) t

mkClosure :: (MonadReader r m, HasEnv r) => Term -> m Closure
mkClosure t = do
    env <- view env_
    return $ Closure env t

vApp :: MonadIO m => Val -> Val -> Icit -> m Val
vApp (VLam _ _ c) u _     = c |@ u
vApp (VFlex m sp) u icit  = return $ VFlex m (sp |> (u, icit))
vApp (VRigid x sp) u icit = return $ VRigid x (sp |> (u, icit))
vApp _ _ _                = error "vApp"

vAppSpine :: MonadIO m => Val -> Spine -> m Val
vAppSpine t SpNil             = return t
vAppSpine t (sp :> (u, icit)) = do
    t' <- vAppSpine t sp
    vApp t' u icit

vMeta :: MonadIO m => MetaVar -> m Val
vMeta m = readMetaVar m <&> (\case
    Solved v _ -> v
    Unsolved _ -> VMeta m)

-- We apply a value to a mask of entries from the environment.
vAppPruning :: MonadIO m => Env -> Val -> Pruning -> m Val
vAppPruning Env.Nil val [] = return val
vAppPruning (env :> t) val (pr :> Just icit) = do
    val' <- vAppPruning env val pr
    vApp val' t icit
vAppPruning (env :> _) val (pr :> Nothing) = vAppPruning env val pr
vAppPruning _ _ _ = error "impossible"

force :: MonadIO m => Val -> m Val
force = \case
  VFlex m sp -> readMetaVar m >>= \case
    Solved t _ -> force =<< vAppSpine t sp
    Unsolved _ -> return $ VFlex m sp
  t -> return t

-- | Convert De Bruijn level to index
lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix l x = l - x - 1

quoteSpine :: MonadIO m => Lvl -> Term -> Spine -> m Term
quoteSpine _ t SpNil             = return t
quoteSpine l t (sp :> (u, icit)) = App <$> quoteSpine l t sp <*> quote l u <*> pure icit

-- | Normalization by evaulation
quote :: MonadIO m => Lvl -> Val -> m Term
quote l (VRigid x sp)    = quoteSpine l (Var (lvl2Ix l x)) sp
quote l (VFlex m sp)     = quoteSpine l (Meta m) sp
quote l (VLam x icit c)  = Lam x icit <$> (quote (l + 1) =<< (c |@ VVar l))
quote l (VPi x icit a c) = Pi x icit <$> quote l a <*> (quote (l + 1) =<< (c |@ VVar l))
quote _ VU               = return U

quote' :: (MonadReader r m, HasEnv r, MonadIO m) => Val -> m Term
quote' v = do
    l <- views env_ Env.level
    quote l v

-- | Normalization by evaulation
nf :: MonadIO m => Env -> Term -> m Term
nf env t = quote (Env.level env) =<< evalTerm env t
