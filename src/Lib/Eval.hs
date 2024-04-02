module Lib.Eval (
    eval,
    (@),
    instClos,
    closeVal,
    vApp,
    vAppSp,
    lvl2Ix,
    force,
    quote,
    quoteSp,
    nf
    ) where

import Control.Applicative
import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Tuple
import GHC.Err
import GHC.Num
import Lib.Common
import Lib.Meta
import Lib.Syntax
import Lib.Value
import Lib.Value.Env            as Env

-- | Evaluation
eval' :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Tm -> ReaderT Env m Val
eval' (Var i) = Env.lookupVal i
eval' (App t u i) = do
    t' <- eval' t
    u' <- eval' u
    lift $ t' `vApp` (u', i)
eval' (AppPrun t pr) = (`vAppPrun` pr) =<< eval' t
eval' (Lam x i t) = VLam x i <$> mkClos t
eval' (Let _ _ t u) = do
    t' <- eval' t
    extendDefined t' (eval' u)
eval' U = return VU
eval' (Pi x i a b) = VPi x i <$> eval' a <*> mkClos b
eval' (Meta m) = vMeta m

eval :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Env -> Tm -> m Val
eval env t = runReaderT (eval' t) env

infixl 8 @

-- | Closure application
(@) :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Clos -> Val -> m Val
Clos env t @ v = runReaderT (extendDefined v $ eval' t) env

instClos :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Clos -> m Val
instClos (Clos env t) = runReaderT (extendBound $ eval' t) env

closeVal :: (MonadReader r f, HasEnv r, HasMetaCtx r, MonadThrow f,  MonadIO f)
    => Env -> Val -> f Clos
closeVal env t = Clos env <$> quote t

vApp :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> (Val, Icit) -> m Val
vApp (VLam _ _ c) (u, _) = c @ u
vApp (VFlex m sp) u      = return $ VFlex m (sp :> u)
vApp (VRigid x sp) u     = return $ VRigid x (sp :> u)
vApp _ _                 = throwString "vApp: impossible"

vAppSp :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> Spine -> m Val
vAppSp t []        = return t
vAppSp t (sp :> u) = t `vAppSp` sp >>= flip vApp u

vMeta :: (MonadIO m) => MVar -> m Val
vMeta m = readMEntry m <&> (\case
    Solved v _ -> v
    Unsolved _ -> VMeta m)

-- | We apply a value to a mask of entries from the environment.
vAppPrun :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> Prun -> ReaderT Env m Val
vAppPrun v pr = do
    env <- view envL
    case (env, pr) of
        (ENil, []) -> return v
        (ECons t env' , Just i : pr') ->
            Env.set env' $ v `vAppPrun` pr' >>= lift . (`vApp` (fst t, i))
        (ECons _ env' , Nothing : pr') -> Env.set env' $ v `vAppPrun` pr'
        _ -> error "impossible"

-- | Convert De Bruijn level to index
lvl2Ix :: (MonadReader e m, HasEnv e) => Lvl -> m Ix
lvl2Ix x = do
    l <- views envL Env.length
    return $ l - x - 1

-- | force
force :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> m Val
force t@(VFlex m sp) = do
    readMEntry m >>= \case
        Solved t' _ -> force =<< t' `vAppSp` sp
        Unsolved _  -> return t
force t = return t

-- | Normalization by evaulation
quote :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> m Tm
quote = quote' <=< force

quote' :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Val -> m Tm
quote' (VFlex m sp)  = quoteSp (Meta m) sp
quote' (VRigid x sp) = Var <$> lvl2Ix x >>= flip quoteSp sp
quote' (VLam x i c)  = Lam x i <$> (quote =<< instClos c)
quote' (VPi x i a c) = Pi x i <$> quote a <*> (quote =<< instClos c)
quote' VU            = return U

quoteSp :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Tm -> Spine -> m Tm
quoteSp t []             = return t
quoteSp t (sp :> (u, i)) = App <$> quoteSp t sp <*> quote u <*> pure i

-- | Normalization by evaulation
nf :: (MonadReader r m, HasEnv r, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Env -> Tm -> m Tm
nf env t = quote =<< eval env t
