module Value (Env(..), Closure(..), VTy, Val(..)) where

import           Common
import           Data.Vector (Vector)
import           Prelude     hiding (length)
import           Syntax


-- | Value environment
newtype Env = Env (Vector Val)
    deriving (Eq, Show, Semigroup, Monoid)

-- | Closure
data Closure = Closure Env Term
    deriving (Eq, Show)

-- | Value types.
type VTy = Val

-- | Values
data Val
    = VVar Lvl
    | VApp Val Val
    | VLam Name Closure
    | VPi Name VTy Closure
    | VU
    deriving (Eq, Show)
