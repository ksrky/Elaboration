module Eval (
    evalTerm,
    evalClosedTerm,
    (|@),
    vApp,
    force,
    lvl2Ix,
    quote,
    nf
    ) where

import           Control.Lens.Cons
import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Meta
import           Syntax
import           Value
import qualified Value.Env              as Env

-- | Evaluation
evalTerm :: MonadIO m => Env -> Term -> m Val
evalTerm env = \case
    Var i -> return $ Env.lookup i env
    App t u ->
         evalTerm env t >>= \case
            VLam _ c -> (c |@) =<< evalTerm env u
            t'       -> vApp t' =<< evalTerm env u
    Lam x t -> return $ VLam x (Closure env t)
    Let _ _ t u -> do
        t' <- evalTerm env t
        evalTerm (Env.append env t') u
    Pi x a b -> do
        a' <- evalTerm env a
        return $ VPi x a' (Closure env b)
    U -> return VU
    Meta mvar -> vMeta mvar
    InsertedMeta mvar envSpecs -> do
        meta <- vMeta mvar
        vAppEnvSpecs env meta envSpecs

evalClosedTerm :: MonadIO m => Term -> m Val
evalClosedTerm = evalTerm Env.empty

infixl 8 |@

-- | Closure application
(|@) :: MonadIO m => Closure -> Val -> m Val
Closure env t |@ v = evalTerm (Env.append env v) t

vApp :: MonadIO m => Val -> Val -> m Val
vApp (VLam _ c) u    = c |@ u
vApp (VFlex m sp) u  = return $ VFlex m (sp |> u)
vApp (VRigid x sp) u = return $ VRigid x (sp |> u)
vApp _ _             = error "vApp"

vAppSpine :: MonadIO m => Val -> Spine -> m Val
vAppSpine t SpNil     = return t
vAppSpine t (sp :> u) = (`vAppSpine` sp) =<< vApp t u

vMeta :: MonadIO m => MetaVar -> m Val
vMeta m = readMetaEntry m <&> (\case
    Solved v -> v
    Unsolved -> VMeta m)

-- We apply a value to a mask of entries from the environment.
vAppEnvSpecs :: MonadIO m => Env -> Val -> [EnvSpec] -> m Val
vAppEnvSpecs env v bds = case (env, bds) of
    (Env.Nil     , []         ) -> return v
    (env :> t , bds :> Bound  ) -> (`vApp` t) =<< vAppEnvSpecs env v bds
    (env :> _ , bds :> Defined) -> vAppEnvSpecs env v bds
    _                           -> error "impossible"

force :: MonadIO m => Val -> m Val
force = \case
  VFlex m sp -> readMetaEntry m >>= \case
    Solved t -> force =<< vAppSpine t sp
    Unsolved -> return $ VFlex m sp
  t -> return t

-- | Convert De Bruijn level to index
lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix l x = l - x - 1

quoteSpine :: MonadIO m => Lvl -> Term -> Spine -> m Term
quoteSpine _ t SpNil     = return t
quoteSpine l t (sp :> u) = App <$> quoteSpine l t sp <*> quote l u

-- | Normalization by evaulation
quote :: MonadIO m => Lvl -> Val -> m Term
quote l (VRigid x sp) = quoteSpine l (Var (lvl2Ix l x)) sp
quote l (VFlex m sp)  = quoteSpine l (Meta m) sp
quote l (VLam x c)    = Lam x <$> (quote (l + 1) =<< (c |@ VVar l))
quote l (VPi x a c)   = Pi x <$> quote l a <*> (quote (l + 1) =<< (c |@ VVar l))
quote _ VU            = return U

-- | Normalization by evaulation
nf :: MonadIO m => Env -> Term -> m Term
nf env t = quote (Env.level env) =<< evalTerm env t
