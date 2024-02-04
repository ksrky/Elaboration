module Eval (weakVar, extEnvWeakVar, eval, (<@>)) where

import           Syntax
import           Value
import           Value.Env as VE
import           Control.Monad.Reader
import           Control.Lens.Combinators   

-- | Get a `Val` by `Ix` from the `Env`.
lookupEnv :: (MonadReader e m, HasValEnv e) => Ix -> m Val
lookupEnv i = views valEnv (! i)

-- | Create a `Clos`.
mkClos :: (MonadReader e m, HasValEnv e) => Tm -> m Clos
mkClos t = do
    env <- view valEnv
    return $ Clos env t

-- | Add a `Val` to the `Env`.
extEnv :: (MonadReader e m, HasValEnv e) => Val -> m a -> m a
extEnv v = locally valEnv (VE.cons v)

-- | Weak variable
weakVar :: (MonadReader e m, HasValEnv e) => m Val
weakVar = VVar <$> views valEnv VE.length

-- | Add a weak variable to the `Env`.
extEnvWeakVar :: (MonadReader e m, HasValEnv e) => m a -> m a
extEnvWeakVar = (weakVar >>=) . flip extEnv

-- | Evaluation
eval :: (MonadReader e m, HasValEnv e) => Tm -> m Val
eval (Var i) = lookupEnv i
eval (App t u) = eval t >>= \case
    VLam _ c -> c <@> eval u
    t'     -> VApp t' <$> eval u
eval (Lam x t) = VLam x <$> mkClos t
eval (Let _ _ t u) = eval t >>= (`extEnv` eval u)
eval U = return VU
eval (Pi x a b) = VPi x <$> eval a <*> mkClos b

infixl 8 <@>

-- | Closure application
(<@>) ::(MonadReader e m, HasValEnv e) => Clos -> m Val -> m Val
Clos env t <@> mv = do
    v <- mv
    locally valEnv (const $ VE.cons v env) $ eval t
