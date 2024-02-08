module Lib.Syntax (Ix, Lvl, Named(..), Ty, Tm(..)) where

import                GHC.Base
import                GHC.Show
import {-# SOURCE #-} Lib.Meta
import                Lib.Raw

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

data Named = Bound | Defined
    deriving (Eq, Show)

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
    | Meta MVar
      -- | Inserted meta
    | IMeta MVar [Named]
    deriving (Eq, Show)
