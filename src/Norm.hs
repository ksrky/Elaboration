module Norm (quote, nf, conv) where

import           Eval
import           Syntax
import           Value
import           Value.Env as VE
import           Control.Monad.Reader
import           Control.Lens.Combinators

-- | Convert De Bruijn level to index
lvl2Ix :: (MonadReader e m, HasValEnv e) => Lvl -> m Ix
lvl2Ix x = do
    l <- views valEnv VE.length
    return $ l - x - 1

-- | Normalization by evaulation
quote :: (MonadReader e m, HasValEnv e) => Val -> m Tm
quote (VVar x)    = Var <$> lvl2Ix x
quote (VApp t u)  = App <$> quote t <*> quote u
quote (VLam x c)  = Lam x <$> (quote =<< c <@> weakVar)
quote VU          = return U
quote (VPi x a c) = Pi x <$> quote a <*> (quote =<< c <@> weakVar)

-- | Normalization by evaulation
nf :: (MonadReader e m, HasValEnv e) => Tm -> m Tm
nf t = quote =<< eval t

-- | Beta-eta conversion checking. Precondition: both values have the same type.
conv :: (MonadReader e m, HasValEnv e, MonadFail m) => Val -> Val -> m ()
conv VU VU = return ()
conv (VPi _ a c) (VPi _ a' c') = do
    conv a a'
    b <- c <@> weakVar
    b' <- c' <@> weakVar
    conv b b'
conv (VLam _ c) (VLam _ c') = do
    t <- c <@> weakVar
    t' <- c' <@> weakVar
    conv t t'
conv (VLam _ c) u = do
    t <- c <@> weakVar
    u' <- VApp u <$> weakVar
    conv t u'
conv t (VLam _ c) = do
    t' <- VApp t <$> weakVar
    u <- c <@> weakVar
    conv t' u
conv (VVar x) (VVar x') | x == x' = return ()
conv (VApp t u) (VApp t' u') = conv t t' >> conv u u'
conv _ _ = fail "conv fail"