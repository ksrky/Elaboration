module Syntax (Ix, Lvl, Tm(..)) where

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Tm
    = Var Ix
    | App Tm Tm
    | Lam Tm
    | Let Tm Tm
