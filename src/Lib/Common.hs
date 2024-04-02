module Lib.Common (Name, Icit(..)) where

import Data.Eq
import Data.String
import GHC.Show

-- | Variable name.
type Name = String

-- | Implicit or explicit.
data Icit = Expl | Impl deriving (Eq, Show)
