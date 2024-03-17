{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Value.Env (
    empty,
    level,
    append,
    increment,
    lookup,
    pattern Nil
    ) where

import           Control.Lens.Cons
import qualified Data.Vector       as Vector
import           Prelude           hiding (lookup)
import           Value

empty :: Env
empty = Env Vector.empty

level :: Env -> Int
level (Env env) = Vector.length env

append :: Env -> Val -> Env
append (Env env) val = Env $ env `Vector.snoc` val

increment :: Env -> Env
increment (Env env) = Env $ env `Vector.snoc` VVar (Vector.length env)

lookup :: Int -> Env -> Val
lookup i (Env env) = env Vector.! i

pattern Nil :: Env
pattern Nil <- Env (Vector.null -> True)

{-# complete Nil, (:>) #-}
