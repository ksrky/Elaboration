module Value (ValEnv(..), Clos(..), VTy, Val(..), HasValEnv(..)) where

import           Raw
import           Syntax
import           Data.Vector (Vector)
import           Prelude hiding (length)
import           Control.Lens.Combinators


-- | Value environment
newtype ValEnv = ValEnv (Vector Val)
    deriving (Eq, Show, Semigroup, Monoid)

-- | Closure
data Clos = Clos ValEnv Tm
    deriving (Eq, Show)

-- | Value types.
type VTy = Val

-- | Values
data Val
    = VVar Lvl
    | VApp Val Val
    | VLam Name Clos
    | VU
    | VPi Name VTy Clos
    deriving (Eq, Show)

class HasValEnv a where
    valEnv :: Lens' a ValEnv