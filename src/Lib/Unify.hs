{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Unify (UnifyError(..), invert, rename, unify) where

import Control.Applicative
import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.IntMap              (IntMap)
import Data.IntMap              qualified as IntMap
import Data.Maybe
import Data.Monoid
import Data.Tuple
import GHC.Base                 hiding (foldr)
import GHC.Num
import GHC.Show
import Lib.Eval
import Lib.Meta
import Lib.Syntax
import Lib.Value
import Lib.Value.Env            as Env

-- | Unification error
newtype UnifyError = UnifyError String
    deriving (Show)

instance Exception UnifyError


-- | partial renaming from Γ to Δ
type Renaming = IntMap Lvl

-- | Lifting a partial renaming over an extra bound variable.
-- Given (σ : PRen Γ Δ), (lift σ : PRen (Γ, x : A[σ]) (Δ, x : A))
liftParRen :: (MonadReader r m, HasEnv r)
    => ReaderT Renaming m a -> ReaderT Renaming m a
liftParRen m = do
    l <- lift $ views envL Env.length
    local (IntMap.insert l l) m

-- | invert : (Γ : Cxt) → (spine : Sub Δ Γ) → ParRen Γ Δ
invert :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m) => Spine -> m Renaming
invert sp = snd <$> foldrM (\t (d, r) -> do
    force t >>= \case
        VVar x | IntMap.notMember x r ->
            return (d + 1, IntMap.insert x d r)
        _ -> throw $ UnifyError "") (0, mempty) sp

-- | perform the partial renaming on rhs, while also checking for "m" occurrences.
rename :: forall r m. (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => MVar -> Val -> ReaderT Renaming m Tm
rename m = go
  where
    goSp :: Tm -> Spine -> ReaderT Renaming m Tm
    goSp t []       = return t
    goSp t (u : sp) = App <$> goSp t sp <*> go u
    go :: Val -> ReaderT Renaming m Tm
    go t = lift (force t) >>= \case
        VFlex m' sp | m == m'   -> throw $ UnifyError "occurs check"
                    | otherwise -> goSp (Meta m') sp
        VRigid x sp -> asks (IntMap.lookup x) >>= \case
            Nothing -> throw $ UnifyError "escaping variable"
            Just x' -> flip goSp sp =<< (Var <$> (lift . lvl2Ix) x')
        VLam x c    -> liftParRen $ Lam x <$> (go =<< (lift . instClos) c)
        VPi x a c   -> liftParRen $ Pi x <$> go a <*> (go =<< (lift . instClos) c)
        VU          -> return U

-- | Wrap a term in lambdas.
lams :: Lvl -> Tm -> Tm
lams l t = foldr (\x t' -> Lam ('x' : show (x + 1)) t') t [0..l - 1]

solve :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => MVar -> Spine -> Val -> m ()
solve m sp rhs = do
    ren <- invert sp
    rhs' <- runReaderT (rename m rhs) ren
    l <- Env.level
    sol <- eval Env.empty $ lams l rhs'
    writeMEntry m (Solved sol)

-- | Unify spines.
unifySp :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Spine -> Spine -> m ()
unifySp [] []               = return ()
unifySp (t : sp) (t' : sp') = unifySp sp sp' >> unify t t'
unifySp _ _                 = throw $ UnifyError ""

-- | Unify values.
unify :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Val -> Val -> m ()
unify t u = do
    t' <- force t
    u' <- force u
    case (t', u') of
        (VLam _ c   , VLam _ c'    ) -> unifyM (instClos c) (instClos c')
        (_          , VLam _ c'    ) -> unifyM (vApp t' =<< weakVar) (instClos c')
        (VLam _ c   , _            ) -> unifyM (instClos c) (vApp t' =<< weakVar)
        (VU         , VU           ) -> return ()
        (VPi _ a c  , VPi _ a' c'  ) -> unify a a' >> unifyM (instClos c) (instClos c')
        (VRigid x sp, VRigid x' sp') | x == x' -> unifySp sp sp'
        (VFlex m sp , VFlex m' sp' ) | m == m' -> unifySp sp sp'
        (VFlex m sp , _            ) -> solve m sp t'
        (_          , VFlex m' sp' ) -> solve m' sp' t'
        _                            -> throwString "throwIO UnifyError"  -- rigid mismatch error

unifyM :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => m Val -> m Val -> m ()
unifyM t u = t >>= \t' -> u >>= \u' -> unify t' u'
