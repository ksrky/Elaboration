module Elab (ElabCtx(..), check) where

import Syntax
import Value
import Value.Env as VE
import Raw
import Control.Monad.Reader
import Control.Lens.Combinators
import Eval
import Norm 

-- type of every variable in scope
type Types = [(Name, VTy)]

-- | Elaboration context.
data ElabCtx = ElabCtx {_valEnv :: ValEnv, _types :: Types, _srcPos :: Int}

instance HasValEnv ElabCtx where
    valEnv = lens _valEnv (\ctx ve -> ctx{_valEnv = ve})

types :: Lens' ElabCtx Types
types = lens _types (\ctx ts -> ctx{_types = ts})

srcPos :: Lens' ElabCtx Int
srcPos = lens _srcPos (\ctx pos -> ctx{_srcPos = pos})

type ElabM = ReaderT ElabCtx

lookupTypes :: MonadFail m => Name -> ElabM m (Tm, VTy)
lookupTypes x = go 0 =<< view types
  where
    go _ [] = fail "variable out of scope"
    go i ((x', a) : tys)
        | x == x' = return (Var i, a)
        | otherwise = go (i + 1) tys

decl :: Monad m => Name -> VTy -> ElabM m a -> ElabM m a
decl x a = extEnvWeakVar . locally types ((x, a):)

define :: Monad m => Name -> Val -> VTy -> ElabM m a -> ElabM m a
define x t a = locally valEnv (VE.cons t) . locally types ((x, a):)

-- | Bidirectional algorithm
check :: MonadFail m => Raw -> VTy -> ElabM m Tm
check (RSrcPos pos t) a = locally srcPos (const pos) $ check t a
check (RLam x t) (VPi _ a c) =
    decl x a $ Lam x <$> (check t =<< (c <@> weakVar))
check (RLet x a t u) b = do
    a' <- check a VU
    va <- eval a'
    t' <- check t va
    vt <- eval t'
    u' <- define x vt va $ check u b
    return $ Let x a' t' u'
check t a = do
    (t', tty) <- infer t
    conv tty a
    pure t'

infer :: MonadFail m => Raw -> ElabM m (Tm, VTy)
infer (RSrcPos pos t) = locally srcPos (const pos) $ infer t
infer (RVar x) = lookupTypes x
infer RU = return (U, VU)
infer (RApp t u) = do
    (t', tty) <- infer t
    case tty of
        VPi _ a c -> do
            u' <- check u a
            (App t' u',) <$> (c <@> eval u')
        _ -> fail "Expected a function type, instead inferred"
infer RLam{} = fail "Can't infer type for lambda expression"
infer (RPi x a b) = do
    a' <- check a VU
    va <- eval a'
    b' <- decl x va $ check b VU
    pure (Pi x a' b', VU)
infer (RLet x a t u) = do
    a' <- check a VU
    va <- eval a'
    t' <- check t va
    vt <- eval t'
    (u', uty) <- define x vt va $ infer u
    pure (Let x a' t' u', uty)
