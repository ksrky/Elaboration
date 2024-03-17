module Raw (Raw(..)) where

import           Common

data Raw
    = Var Name
    | Lam Name Raw
    | App Raw Raw
    | U
    | Pi Name Raw Raw
    | Let Name Raw Raw Raw
    | SrcPos Int Raw
    deriving (Eq, Show)
