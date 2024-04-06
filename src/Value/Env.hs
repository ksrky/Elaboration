{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Value.Env (
    empty,
    level,
    append,
    increment,
    lookup,
    fromList,
    pattern Nil
    ) where

import Control.Lens.Cons
import Data.Vector       qualified as Vector
import Prelude           hiding (lookup)
import Value

empty :: Env
empty = Vector.empty

level :: Env -> Int
level = Vector.length

append :: Env -> Val -> Env
append = Vector.snoc

increment :: Env -> Env
increment env = env `Vector.snoc` VVar (Vector.length env)

lookup :: Int -> Env -> Val
lookup i env = env Vector.! i

fromList :: [Val] -> Env
fromList = Vector.fromList

pattern Nil :: Env
pattern Nil <- (Vector.null -> True)

{-# complete Nil, (:>) #-}
