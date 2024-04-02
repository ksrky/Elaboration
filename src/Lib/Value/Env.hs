module Lib.Value.Env (
    length,
    empty,
    cons,
    (!),
    lookup,
    lookupVal,
    extend,
    extendBound,
    extendDefined,
    set,
    level,
    mkClos,
    weakVar
    ) where

import Control.Lens.Combinators   hiding (cons, set)
import Control.Monad
import Control.Monad.Reader.Class
import Data.Function
import Data.Functor
import Data.Int
import Data.Tuple
import Data.Vector                qualified as Vector
import Lib.Syntax
import Lib.Value

-- * Basic operations

length :: Env -> Int
length (Env env) = Vector.length env

empty :: Env
empty = Env Vector.empty

cons :: (Val, Named) -> Env -> Env
cons v (Env env) = Env $ v `Vector.cons` env

(!) ::  Env -> Int -> (Val, Named)
Env env ! i = env Vector.! i

-- * Monadic operations

-- ** Environment management

-- | Get a `Val` by `Ix` from the `Env`.
lookup :: (MonadReader r m, HasEnv r) => Ix -> m (Val, Named)
lookup i = views envL (! i)

lookupVal :: (MonadReader r m, HasEnv r) => Ix -> m Val
lookupVal i = fst <$> lookup i

-- | Add a `Val` to the `Env`.
extend :: (MonadReader r m, HasEnv r) => (Val, Named) -> m a -> m a
extend v = locally envL (cons v)

-- | Set the `Env`.
set :: (MonadReader r m, HasEnv r) => Env -> m a -> m a
set env = locally envL (const env)

extendBound :: (MonadReader r m, HasEnv r) => m a -> m a
extendBound m = do
    l <- views envL length
    extend (VVar l, Bound) m

extendDefined :: (MonadReader r m, HasEnv r) => Val -> m a -> m a
extendDefined v = extend (v, Defined)

-- ** Utilities
-- get current level
level :: (MonadReader r m, HasEnv r) => m Lvl
level = views envL length

-- | Create a `Clos`.
mkClos :: (MonadReader r m, HasEnv r) => Tm -> m Clos
mkClos t = do
    env <- view envL
    return $ Clos env t

-- | Weak variable
weakVar :: (MonadReader r m, HasEnv r) => m Val
weakVar = VVar <$> views envL length
