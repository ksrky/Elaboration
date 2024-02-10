{-# LANGUAGE DataKinds #-}

module Lib.Raw (ArgInfo, SrcPos, Raw(..)) where

import Data.Either
import Data.Eq
import Data.Int
import GHC.Show
import Lib.Common

-- | Argument information
type ArgInfo = Either Name Icit

-- | Source position
type SrcPos = (Int, Int)

data Raw
    = -- | @x@
      RVar Name
    | -- | @λx → t@ | @λ{x} → t@ | @λ{l = x} → t@
      RLam Name ArgInfo Raw
    | -- | @t u@ | @t {u}@ | @t {x = u}@
      RApp Raw Raw ArgInfo
    | -- | @U@
      RU
    | -- | @(x : a) → b@ | @{x : a} → b@ | @{l = x : a} → b@
      RPi Name Icit Raw Raw
    | -- | @let x : a = t in u@
      RLet Name Raw Raw Raw
    | -- | @_@
      RHole
      -- | @t@ with source position
    | RSrcPos SrcPos Raw
    deriving (Eq, Show)
