module Lib.Syntax (Ix, Lvl, Named(..), Prun, Ty, Tm(..)) where


import                Data.Eq
import                Data.Int
import                Data.Maybe
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

-- | Pruning
type Prun = [Maybe Icit]

-- | Types.
type Ty = Tm

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Tm
    -- | Variable
    = Var Ix
      -- | Application
    | App Tm Tm Icit
      -- | Application with inserted or pruned meta
    | AppPrun Tm Prun
      -- | Lambda abstraction
    | Lam Name Icit Tm
      -- | Let expression
    | Let Name Ty Tm Tm
      -- | Universe
    | U
      -- | Dependent function type
    | Pi Name Icit Ty Ty
      -- | Meta variable
    | Meta MVar
    deriving (Eq, Show)
