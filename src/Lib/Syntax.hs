module Lib.Syntax (Ix, Lvl, Named(..), Ty, Tm(..)) where

import                GHC.Base
import                GHC.Show
import                Lib.Common
import {-# SOURCE #-} Lib.Meta

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

-- | Bound or Defined.
data Named = Bound | Defined
    deriving (Eq, Show)

-- | Types.
type Ty = Tm

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Tm
    = Var Ix
    | App Tm Tm Icit
    | Lam Name Icit Tm
    | Let Name Ty Tm Tm
    | U
    | Pi Name Icit Ty Ty
    | Meta MVar
      -- | Inserted meta
    | IMeta MVar [Named]
    deriving (Eq, Show)
