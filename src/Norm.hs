module Norm (quote, nf) where

import           Eval
import           Syntax
import           Value

-- | Convert De Bruijn level to index
lvl2Ix :: Int -> Lvl -> Ix
lvl2Ix l x = l - x - 1

-- | Normalization by evaulation
quote :: Int -> Val -> Tm
quote l (VVar x)   = Var (lvl2Ix l x)
quote l (VApp t u) = App (quote l t) (quote l u)
quote l (VLam c)   = Lam (quote (l + 1) (c <@> VVar l))

-- | Normalization by evaulation
nf :: Env -> Tm -> Tm
nf env t = quote (length env) (eval env t)
