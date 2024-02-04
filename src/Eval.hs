{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Eval (eval, (<@>)) where

import           Data.Sized
import           Data.Type.Natural
import           Syntax
import           Value

-- | Evaluation
eval :: KnownNat n => Env n -> Tm n -> Val
eval env (Var i) = env %!! i
eval env (App t u) = case eval env t of
    VLam c -> c <@> eval env u
    t'     -> VApp t' (eval env u)
eval env (Lam t) = VLam (Closure env t)
eval env (Let t u) = eval (eval env t <| env) u

-- | Closure application
(<@>) :: Closure -> Val -> Val
Closure env t <@> v = eval (v <| env) t
