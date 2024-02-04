module Value.Env (length, cons, (!)) where

import Value
import Prelude hiding (length)
import qualified Data.Vector as Vector

length :: ValEnv -> Int
length (ValEnv env) = Vector.length env

cons :: Val -> ValEnv -> ValEnv
cons v (ValEnv env) = ValEnv $ v `Vector.cons` env

(!) ::  ValEnv -> Int -> Val
ValEnv env ! i = env Vector.! i 
