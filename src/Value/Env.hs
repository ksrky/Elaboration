module Value.Env (level, append, increment, lookup) where

import qualified Data.Vector as Vector
import           Prelude     hiding (length, lookup)
import           Value

level :: Env -> Int
level (Env env) = Vector.length env

append :: Env -> Val -> Env
append (Env env) val = Env $ env `Vector.snoc` val

increment :: Env -> Env
increment (Env env) = Env $ env `Vector.snoc` VVar (Vector.length env)

lookup :: Int -> Env -> Val
lookup i (Env env) = env Vector.! i
