module Eval (
    evalTerm,
    evalClosedTerm,
    (|@)
    ) where

import           Syntax
import           Value
import qualified Value.Env as Env

-- | Evaluation
evalTerm :: Env -> Term -> Val
evalTerm env = \case
    Var i -> Env.lookup i env
    App t u ->
        case evalTerm env t of
            VLam _ c -> c |@ evalTerm env u
            t'       -> VApp t' (evalTerm env u)
    Lam x t -> VLam x (Closure env t)
    Let _ _ t u -> evalTerm (Env.append env (evalTerm env t)) u
    Pi x a b -> VPi x (evalTerm env a) (Closure env b)
    U -> VU

evalClosedTerm :: Term -> Val
evalClosedTerm = evalTerm Env.empty

infixl 8 |@

-- | Closure application
(|@) :: Closure -> Val -> Val
Closure env t |@ v = evalTerm (Env.append env v) t
