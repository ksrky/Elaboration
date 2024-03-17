{-# LANGUAGE PatternSynonyms #-}

module Value (
    Env(..),
    Spine(unSpine),
    pattern SpNil,
    Closure(..),
    VTy,
    Val(..),
    pattern VVar,
    pattern VMeta
    ) where

import                          Common
import                          Control.Lens.Combinators
import                          Control.Lens.Cons
import                          Data.Vector              (Vector)
import {-# SOURCE #-}           Meta
import                          Prelude                  hiding (length)
import                          Syntax


-- | Value environment
newtype Env = Env (Vector Val)
    deriving (Eq, Show, Semigroup, Monoid)

instance Snoc Env Env Val Val where
    _Snoc = prism
        (\(Env env, val) -> Env (env `snoc` val))
        (\(Env env) -> case unsnoc env of
            Nothing       -> Left (Env mempty)
            Just (env', v) -> Right (Env env', v))

-- | Spine
newtype Spine = Spine {unSpine :: [Val]}
    deriving (Eq, Show, Semigroup, Monoid)

pattern SpNil :: Spine
pattern SpNil = Spine []

instance Snoc Spine Spine Val Val where
    _Snoc = prism
        (\(Spine xs, x) -> Spine (x : xs))
        (\(Spine xs) -> case uncons xs of
            Nothing       -> Left (Spine [])
            Just (x, xs') -> Right (Spine xs', x))

{-# complete SpNil, (:>) #-}

-- | Closure
data Closure = Closure Env Term
    deriving (Eq, Show)

-- | Value types.
type VTy = Val

-- | Values
data Val
    = VRigid Lvl Spine
    | VFlex MetaVar Spine
    | VLam Name Closure
    | VPi Name VTy Closure
    | VU
    deriving (Eq, Show)

pattern VVar :: Lvl -> Val
pattern VVar l = VRigid l (Spine [])

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m (Spine [])
