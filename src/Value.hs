{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Value (Env, Closure(..), Val(..)) where

import           Syntax

-- | Environment
type Env = [Val]

-- | Closure
data Closure = Closure Env Tm

-- | Values
data Val
    = VVar Lvl
    | VApp Val Val
    | VLam Closure
