{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Value (Env, envLength, Closure(..), Val(..)) where

import           Data.Sized
import           Data.Type.Natural
import           Syntax

-- | Environment
type Env n = Sized [] n Val

envLength :: KnownNat n => Env n -> SNat n
envLength = sLength

-- | Closure
data Closure where
    Closure :: KnownNat n => Env n -> Tm (n + 1) -> Closure

-- | Value
--
-- [Note] De brujin levels are statically unknown during evaluation
-- because evaluation of `Var` decreases Environment length
-- and `Lam` and `Let` increases it.
data Val
    = VVar Lvl
    | VApp Val Val
    | VLam Closure
