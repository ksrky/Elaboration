module Lib.Raw (Name, Raw(..)) where

import GHC.Base
import GHC.Show

-- | Variable name.
type Name = String

data Raw
    = -- | @x@
      RVar Name
    | -- | @λx → t@
      RLam Name Raw
    | -- | @t u@
      RApp Raw Raw
    | -- | @U@
      RU
    | -- | @(x : a) → b@
      RPi Name Raw Raw
    | -- | @let x : a = t in u@
      RLet Name Raw Raw Raw
    | -- | @_@
      RHole
      -- | @t@ with source position
    | RSrcPos Int Raw
    deriving (Eq, Show)
