module Lib.Eval (
    eval,
    (@),
    instClos,
    vApp,
    vAppSp,
    lvl2Ix,
    force,
    quote,
    quoteSp,
    nf
    ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Monad.Reader
import Data.Function
import Data.Functor
import Data.Tuple
import GHC.Base
import GHC.Num
import Lib.Meta
import Lib.Syntax
import Lib.Value
import Lib.Value.Env            as Env

-- | Evaluation
eval' :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Tm -> ReaderT Env m Val
eval' (Var i) = Env.lookupVal i
eval' (App t u) = do
    t' <- eval' t
    u' <- eval' u
    lift $ vApp t' u'
eval' (Lam x t) = VLam x <$> mkClos t
eval' (Let _ _ t u) = do
    t' <- eval' t
    extendDefined t' (eval' u)
eval' U = return VU
eval' (Pi x a b) = VPi x <$> eval' a <*> mkClos b
eval' (Meta m) = vMeta m
eval' (IMeta m ns) = vMeta m >>= flip vAppNameds ns

eval :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Env -> Tm -> m Val
eval env t = runReaderT (eval' t) env

infixl 8 @

-- | Closure application
(@) :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Clos -> Val -> m Val
Clos env t @ v = runReaderT (extendDefined v $ eval' t) env

instClos :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Clos -> m Val
instClos (Clos env t) = runReaderT (extendBound $ eval' t) env

vApp :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> Val -> m Val
vApp (VLam _ c) u    = c @ u
vApp (VFlex m sp) u  = return $ VFlex m (u : sp)
vApp (VRigid x sp) u = return $ VRigid x (u : sp)
vApp _ _             = fail "vApp: impossible"

vAppSp :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> Spine -> m Val
vAppSp t []       = return t
vAppSp t (u : sp) = t `vAppSp` sp >>= flip vApp u

vMeta :: (MonadIO m) => MVar -> m Val
vMeta m = readMEntry m <&> (\case
    Solved v -> v
    Unsolved -> VMeta m)

-- | We apply a value to a mask of entries from the environment.
vAppNameds :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> [Named] -> ReaderT Env m Val
vAppNameds v ns = do
    env <- view envL
    case (env, ns) of
        (ENil, []) -> return v
        (ECons t env' , Bound : ns') ->
            Env.set env' $ v `vAppNameds` ns' >>= lift . flip vApp (fst t)
        (ECons _ env' , Defined : ns') ->
            Env.set env' $ v `vAppNameds` ns'
        _ -> error "impossible"

-- | Convert De Bruijn level to index
lvl2Ix :: (MonadReader e m, HasEnv e) => Lvl -> m Ix
lvl2Ix x = do
    l <- views envL Env.length
    return $ l - x - 1

-- | force
force :: (MonadReader r m, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> m Val
force t@(VFlex m sp) = do
    readMEntry m >>= \case
        Solved t' -> force =<< vAppSp t' sp
        Unsolved -> return t
force t = return t

-- | Normalization by evaulation
quote :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> m Tm
quote = quote' <=< force

quote' :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadFail m, MonadIO m)
    => Val -> m Tm
quote' (VFlex m sp)  = quoteSp (Meta m) sp
quote' (VRigid x sp) = Var <$> lvl2Ix x >>= flip quoteSp sp
quote' (VLam x c)    = Lam x <$> (quote =<< instClos c)
quote' (VPi x a c)   = Pi x <$> quote a <*> (quote =<< instClos c)
quote' VU            = return U

quoteSp :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadFail m, MonadIO m)
    => Tm -> Spine -> m Tm
quoteSp t []       = return t
quoteSp t (u : sp) = App <$> quoteSp t sp <*> quote u

-- | Normalization by evaulation
nf :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadFail m, MonadIO m)
    => Env -> Tm -> m Tm
nf env t = quote =<< eval env t
