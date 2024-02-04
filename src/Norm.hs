{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Norm (quote, nf) where


import           Data.Type.Natural
import           Data.Type.Ordinal
import           Eval
import           Syntax
import           Value

-- | Convert De Bruijn level to index
lvl2Ix :: KnownNat n => SNat n -> Lvl -> Ix n
lvl2Ix l (SomeSNat x) = sNatToOrd (sPred (l %- x)) -- TODO

-- | Normalization by evaulation
quote :: KnownNat n => SNat n -> Val -> Tm n
quote l (VVar x)   = Var (lvl2Ix l x)
quote l (VApp t u) = App (quote l t) (quote l u)
quote l (VLam c)   = Lam (quote (sS l) (c <@> VVar (SomeSNat l)))

-- | Normalization by evaulation
nf :: KnownNat n => Env n -> Tm n -> Tm n
nf env t = quote (envLength env) (eval env t)
