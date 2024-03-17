module Norm (quote, nf, conv) where

import           Eval
import           Syntax
import           Value
import           Value.Env as Env

-- | Convert De Bruijn level to index
lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix l x = l - x - 1

-- | Normalization by evaulation
quote :: Lvl -> Val -> Term
quote l (VVar x)    = Var (lvl2Ix l x)
quote l (VApp t u)  = App (quote l t) (quote l u)
quote l (VLam x c)  = Lam x (quote (l + 1) (c |@ VVar l))
quote _ VU          = U
quote l (VPi x a c) = Pi x (quote l a) (quote (l + 1) (c |@ VVar l))

-- | Normalization by evaulation
nf :: Env -> Term -> Term
nf env t = quote (Env.level env) (evalTerm env t)

-- | Beta-eta conversion checking. Precondition: both values have the same type.
conv :: MonadFail m => Lvl -> Val -> Val -> m ()
conv _ VU VU = return ()
conv l (VPi _ a c) (VPi _ a' c') = do
    conv l a a'
    conv (l + 1) (c |@ VVar l) (c' |@ VVar l)
conv l (VLam _ c) (VLam _ c') =
    conv (l + 1) (c |@ VVar l) (c' |@ VVar l)
conv l (VLam _ c) u = do
    conv (l + 1) (c |@ VVar l) (VApp u (VVar l))
conv l t (VLam _ c) =
    conv (l + 1) (VApp t (VVar l)) (c |@ VVar l)
conv _ (VVar x) (VVar x') | x == x' = return ()
conv l (VApp t u) (VApp t' u') = do
    conv l t t'
    conv l u u'
conv _ _ _ = fail "conv fail"
