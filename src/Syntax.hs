module Syntax (Ix, Lvl, Ty, Tm(..)) where

import Raw

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

-- | Types.
type Ty = Tm

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Tm
    = Var Ix
    | App Tm Tm
    | Lam Name Tm
    | Let Name Ty Tm Tm
    | U
    | Pi Name Ty Ty
    deriving (Eq, Show)
