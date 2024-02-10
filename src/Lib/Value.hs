{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib.Value (
    Env(..),
    pattern ENil,
    pattern ECons,
    HasEnv(..),
    Spine,
    pattern (:>),
    Clos(..),
    VTy,
    Val(..),
    pattern VVar,
    pattern VMeta) where

import                Control.Lens.Combinators
import                Data.Vector              (Vector)
import                Data.Vector              qualified as Vector
import                Lib.Common
import {-# SOURCE #-} Lib.Meta
import                Lib.Syntax
import                Prelude                  hiding (length)


-- | Value environment
newtype Env = Env (Vector (Val, Named))
    deriving (Eq, Show, Semigroup, Monoid)

unconsEnv :: Env -> Maybe ((Val, Named), Env)
unconsEnv (Env vs) = case Vector.uncons vs of
    Nothing       -> Nothing
    Just (v, vs') -> Just (v, Env vs')

pattern ENil :: Env
pattern ENil <- (unconsEnv -> Nothing)

pattern ECons :: (Val, Named) -> Env -> Env
pattern ECons v vs <- (unconsEnv -> Just (v, vs))

class HasEnv a where
    envL :: Lens' a Env

instance HasEnv Env where
    envL = id

-- | Spine
type Spine   = [(Val, Icit)]

pattern (:>) :: Spine -> (Val, Icit) -> Spine
pattern xs :> x = x : xs
{-# COMPLETE (:>), [] #-}

-- | Closure
data Clos = Clos Env Tm
    deriving (Eq, Show)

-- | Value types.
type VTy = Val

-- | Values
data Val
    = -- | A flexible neutral value is a meta applied to zero or more arguments,
      -- represent as a Spine, a snoc-list of values.
      VFlex MVar Spine
      -- | A rigid neutral value is a bound variable applied to zero or
      -- more arguments
    | VRigid Lvl Spine
    | VLam Name Icit Clos
    | VPi Name Icit VTy Clos
    | VU
    deriving (Eq, Show)

pattern VVar :: Lvl -> Val
pattern VVar  x = VRigid x []

pattern VMeta :: MVar -> Val
pattern VMeta m = VFlex m []
