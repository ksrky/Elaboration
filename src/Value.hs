{-# LANGUAGE PatternSynonyms #-}

module Value (
    Env(..),
    Spine,
    pattern SpNil,
    spineLength,
    Closure(..),
    ValTy,
    Val(..),
    pattern VVar,
    pattern VMeta
    ) where

import                          Common
import                          Control.Lens.Combinators
import                          Control.Lens.Cons
import                          Data.Vector              (Vector)
import {-# SOURCE #-}           Meta
import                          Syntax


-- | Value environment
newtype Env = Env (Vector Val)
    deriving (Eq, Show, Semigroup, Monoid)

instance Snoc Env Env Val Val where
    _Snoc = prism
        (\(Env env, val) -> Env (env `snoc` val))
        (\(Env env) -> case unsnoc env of
            Nothing        -> Left (Env mempty)
            Just (env', v) -> Right (Env env', v))

-- | Spine
type Spine = [(Val, Icit)]

pattern SpNil :: Spine
pattern SpNil = []

{-# complete SpNil, (:>) #-}

spineLength :: Spine -> Lvl
spineLength = length

-- | Closure
data Closure = Closure Env Term
    deriving (Eq, Show)

-- | Value types.
type ValTy = Val

-- | Values
data Val
    = VRigid Lvl Spine
    | VFlex MetaVar Spine
    | VLam Name Icit Closure
    | VPi Name Icit ValTy Closure
    | VU
    deriving (Eq, Show)

pattern VVar :: Lvl -> Val
pattern VVar l = VRigid l []

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m []
