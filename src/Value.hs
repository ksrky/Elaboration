{-# LANGUAGE PatternSynonyms #-}

module Value
    ( Env
    , HasEnv(..)
    , Spine
    , pattern SpNil
    , Closure(..)
    , ValTy
    , Val(..)
    , pattern VVar
    , pattern VMeta
    ) where

import                Common
import                Control.Lens.Combinators
import                Control.Lens.Cons
import                Data.Vector              (Vector)
import {-# SOURCE #-} Meta
import                Syntax


-- | Value environment.
type Env = Vector Val

class HasEnv a where
    env_ :: Lens' a Env

instance HasEnv Env where
    env_ = id

-- | Spine.
type Spine = [(Val, Icit)]

-- | Empty spine.
pattern SpNil :: Spine
pattern SpNil = []

{-# complete SpNil, (:>) #-}

-- | Closure
data Closure = Closure Env Term
    deriving (Eq, Show)

-- | Value types.
type ValTy = Val

-- | Values
data Val
    = VRigid Lvl Spine
    | VFlex  UnsolvedMetaVar Spine
    | VLam   Name Icit Closure
    | VPi    Name Icit ValTy Closure
    | VU
    deriving (Eq, Show)

pattern VVar :: Lvl -> Val
pattern VVar l = VRigid l []

pattern VMeta :: UnsolvedMetaVar -> Val
pattern VMeta m = VFlex m []
