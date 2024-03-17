module Eval (
    eval,
    (|@)
    ) where

import           Syntax
import           Value
import qualified Value.Env as Env

-- | Evaluation
eval :: Env -> Term -> Val
eval env = \case
    Var i -> Env.lookup i env
    App t u ->
        case eval env t of
            VLam _ c -> c |@ eval env u
            t'       -> VApp t' (eval env u)
    Lam x t -> VLam x (Closure env t)
    Let _ _ t u -> eval (Env.append env (eval env t)) u
    Pi x a b -> VPi x (eval env a) (Closure env b)
    U -> VU

infixl 8 |@

-- | Closure application
(|@) :: Closure -> Val -> Val
Closure env t |@ v = eval (Env.append env v) t
