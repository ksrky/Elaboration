{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Syntax (Ix, Lvl, Tm(..)) where

import           Data.Type.Natural
import           Data.Type.Ordinal

-- | De Bruijn index @Ix n@ is between 0 and n-1
type Ix n = Ordinal n

-- | De Bruijn level
type Lvl = SomeSNat

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Tm (n :: Nat)
    = Var (Ix n)
    | App (Tm n) (Tm n)
    | Lam (Tm (n + 1))
    | Let (Tm n) (Tm (n + 1))
