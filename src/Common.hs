module Common (Name, Icit(..)) where

-- | Variable name.
type Name = String

-- | Explicit or implicit.
data Icit = Expl | Impl
    deriving (Eq, Show)
