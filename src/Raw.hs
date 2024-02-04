module Raw (Name, Raw(..)) where

-- | Variable name.
type Name = String

data Raw
    = RVar Name
    | RLam Name Raw
    | RApp Raw Raw
    | RU
    | RPi Name Raw Raw
    | RLet Name Raw Raw Raw
    | RSrcPos Int Raw
    deriving (Eq, Show)
