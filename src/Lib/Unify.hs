{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib.Unify (UnifyError(..), invert, rename, unify) where

import Control.Applicative
import Control.Exception.Safe
import Control.Lens.Combinators
import Control.Monad
import Control.Monad.Reader
import Data.Bool
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Int
import Data.IntMap              (IntMap)
import Data.IntMap              qualified as IntMap
import Data.List                qualified as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.String
import Data.Traversable
import GHC.Err
import GHC.Num
import GHC.Show
import Lib.Common
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
data ParRen = ParRen {
    _occvar   :: Maybe MVar,
    _renaming :: IntMap Lvl
    }
    deriving (Eq, Show)

makeLenses ''ParRen

-- | Lifting a partial renaming over an extra bound variable.
-- Given (σ : PRen Γ Δ), (lift σ : PRen (Γ, x : A[σ]) (Δ, x : A))
liftParRen :: (MonadReader r m, HasEnv r) => ReaderT ParRen m a -> ReaderT ParRen m a
liftParRen m = do
    l <- lift $ views envL Env.length
    locally renaming (IntMap.insert l l) m

-- | invert : (Γ : Cxt) → (spine : Sub Δ Γ) → ParRen Γ Δ
invert :: (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Spine -> m (ParRen, Maybe Prun)
invert sp = do
    (_, ren, nls, fsp) <- foldrM (\(t, i) (dom, ren, nls, fsp) -> do
        force t >>= \case
            VVar x | IntMap.notMember x ren || x `elem` nls ->
                return (dom + 1, IntMap.insert x dom ren, x : nls, (x, i) : fsp)
            VVar x -> return (dom + 1, IntMap.insert x dom ren, nls, (x, i) : fsp)
            _ -> throw $ UnifyError "") (0, mempty, mempty, mempty) sp
    let pr = List.map (\case
            (x, _) | x `elem` nls -> Nothing
            (_, i) -> Just i) fsp
    return (ParRen Nothing ren, pr <$ guard (not $ List.null nls))

-- | Wrap a term in lambdas.
mkLams :: forall r m. (MonadReader r m, HasMetaCtx r, MonadThrow m, MonadIO m)
    => Int -> VTy -> Tm -> m Tm
mkLams n a t = go n a
  where
    go :: Int -> VTy -> m Tm
    go 0 _  = return t
    go k a' = force a' >>= \case
        VPi "_" i _ b -> Lam "_" i <$> (go (k - 1) =<< instClos b)
        VPi x i _ b   -> Lam x i <$> (go (k - 1) =<< instClos b)
        _ -> error "impossible"

pruneTy :: forall r m. (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Prun -> VTy -> m Ty
pruneTy pr ty = runReaderT (aux (List.reverse pr) ty) (ParRen Nothing mempty)
  where
    aux :: Prun -> VTy -> ReaderT ParRen m Tm
    aux pr1 ty1 = (lift . force) ty1 >>= \ty' -> case (pr1, ty') of
        ([], _) -> rename ty'
        (Just _ : pr', VPi x i a c) ->
            liftParRen $ Pi x i <$> rename a <*> (aux pr' =<< (lift . instClos) c)
        (Nothing : pr', VPi _ _ _ c) -> aux pr' =<< (lift . instClos) c
        _ -> error "impossible"

pruneMeta :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Prun -> MVar -> m MVar
pruneMeta pr m = do
    mty <- readMEntry m >>= \case
        Unsolved a -> return a
        _          -> error "impossible"
    pruned <- eval Env.empty =<< pruneTy pr mty
    m' <- newMVar pruned
    sol <- eval Env.empty =<< mkLams (List.length pr) mty (AppPrun (Meta m') pr)
    writeMEntry m (Solved sol mty)
    return m'

data SpinePruneStatus
    = OKRenaming    -- ^ Valid spine which is a renaming
    | OKNonRenaming -- ^ Valid spine but not a renaming (has a non-var entry)
    | NeedsPruning  -- ^ A spine which is a renaming and has out-of-scope var entries
    deriving (Eq, Show)

-- | Prune illegal var occurrences from a meta + spine.
--   Returns: renamed + pruned term.
pruneVFlex :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => MVar -> Spine -> ReaderT ParRen m Tm
pruneVFlex m sp = do
    (status, sp') <- forAccumM OKRenaming sp $ \s (t, i) -> (lift . force) t >>= \case
        VVar x -> IntMap.lookup x & views renaming >>= \case
            Just x' -> (s,) . (,i) . Just . Var <$> (lift . lvl2Ix) x'
            Nothing | s == OKNonRenaming -> throw $ UnifyError ""
            Nothing -> return (NeedsPruning, (Nothing, i))
        _ | s == NeedsPruning -> throw $ UnifyError ""
        _                     -> (OKNonRenaming,) . (,i) . Just <$> rename t
    m' <- case status of
        OKRenaming    -> readMEntry m >>= \case Unsolved _ -> return m; _ -> error "impossible"
        OKNonRenaming -> readMEntry m >>= \case Unsolved _ -> pure m; _ -> error "impossible"
        NeedsPruning  -> lift $ pruneMeta (List.map (\(mt, i) -> i <$ mt) sp') m
    return $ foldr (\(mu, i) t -> maybe t (\u -> App t u i) mu) (Meta m') sp'

renameSp :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Tm -> Spine -> ReaderT ParRen m Tm
renameSp t [] = return t
renameSp t (sp :> (u, i)) = do
    App <$> renameSp t sp <*> rename u <*> pure i

-- | perform the partial renaming on rhs, while also checking for "m" occurrences.
rename :: forall r m. (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => Val -> ReaderT ParRen m Tm
rename t = lift (force t) >>= \case
    VFlex m' sp -> view occvar >>= \case
        Just m | m == m' -> throw $ UnifyError "occurs check"
        _                -> pruneVFlex m' sp
    VRigid x sp -> views renaming (IntMap.lookup x) >>= \case
        Nothing -> throw $ UnifyError "escaping variable"
        Just x' -> flip renameSp sp =<< (Var <$> (lift . lvl2Ix) x')
    VLam x i c  -> liftParRen $ Lam x i <$> (rename =<< (lift . instClos) c)
    VPi x i a c -> liftParRen $ Pi x i <$> rename a <*> (rename =<< (lift . instClos) c)
    VU          -> return U

-- | Solve m given the result of inversion on a spine.
solveWithParRen :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => MVar -> (ParRen, Maybe Prun) -> Val -> m ()
solveWithParRen m (pren, pruneNonlinear) rhs = do
    mty <- readMEntry m >>= \case
        Unsolved a -> pure a
        _          -> error "impossible"
    -- if the spine was non-linear, we check that the non-linear arguments
    -- can be pruned from the meta type (i.e. that the pruned solution will
    -- be well-typed)
    case pruneNonlinear of
        Nothing -> return ()
        Just pr -> do
            void $ pruneTy pr mty
            rhs' <- runReaderT (rename rhs) (pren{_occvar = Just m})
            sol <- eval Env.empty undefined -- mkLams mty rhs
            writeMEntry m (Solved sol mty)

solve :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadThrow m, MonadIO m)
    => MVar -> Spine -> Val -> m ()
solve m sp rhs = do
    (pren, mbpr) <- invert sp
    solveWithParRen m (pren, mbpr) rhs

-- | Unify spines.
unifySp :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadCatch m, MonadIO m)
    => Spine -> Spine -> m ()
unifySp [] []                           = return ()
unifySp (sp :> (t, _)) (sp' :> (t', _)) = unifySp sp sp' >> unify t t'
unifySp _ _                             = throw $ UnifyError "rigid mismatch error"

-- | Solve (Γ ⊢ m spine =? m' spine').
flexFlex :: forall r m. (MonadReader r m, HasMetaCtx r, HasEnv r, MonadCatch m, MonadIO m)
    => MVar -> Spine -> MVar -> Spine -> m ()
flexFlex m sp m' sp' =
    -- usually, a longer spine indicates that the meta is in an inner scope. If we solve
    -- inner metas with outer metas, that means that we have to do less pruning.
    if List.length sp < List.length sp'
            then go m' sp' m sp
            else go m sp m' sp'
  where
    -- It may be that only one of the two spines is invertible
    go :: MVar -> Spine -> MVar -> Spine -> m ()
    go m1 sp1 m1' sp1' = (do
        pren <- invert sp1
        solveWithParRen m1 pren (VFlex m1' sp1'))
        `catch` \case
            UnifyError _ -> solve m1' sp1' (VFlex m1 sp1)

-- | Try to solve the problem (Γ ⊢ m spine =? m spine') by intersection.
--   If spine and spine' are both renamings, but different, then
--   we prune all arguments from m which differ in spine and spine'.
--
--   If some of spine/spine' are not renamings, we fall back to simple unification.
intersect :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadCatch m, MonadIO m)
    => MVar -> Spine -> Spine -> m ()
intersect m sp sp' = do
    mbpr <- sequence <$> zipWithM (\(t, i) (t', _) -> do
        t1 <- force t
        t1' <- force t'
        return $ case (t1, t1') of
            (VVar x, VVar x') | x == x' -> pure (Just i)
            _                           -> Nothing
        ) sp sp'
    case mbpr of
        Nothing                     -> unifySp sp sp'
        Just pr | Nothing `elem` pr -> void $ pruneMeta pr m
                | otherwise         -> return ()

-- | Unify values.
unify :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadCatch m, MonadIO m)
    => Val -> Val -> m ()
unify t u = do
    t' <- force t
    u' <- force u
    case (t', u') of
        (VLam _ _ c , VLam _ _ c'  ) -> unifyM (instClos c) (instClos c')
        (_          , VLam _ i' c' ) -> unifyM (vApp t' =<< (,i') <$> weakVar) (instClos c')
        (VLam _ i c , _            ) -> unifyM (instClos c) (vApp t' =<< (,i) <$> weakVar)
        (VU         , VU           ) -> return ()
        (VPi _ _ a c, VPi _ _ a' c') -> unify a a' >> unifyM (instClos c) (instClos c')
        (VRigid x sp, VRigid x' sp') | x == x' -> unifySp sp sp'
        (VFlex m sp , VFlex m' sp' ) | m == m' -> intersect m sp sp'
        (VFlex m sp , VFlex m' sp' )           -> flexFlex m sp m' sp'
        (VFlex m sp , _            ) -> solve m sp u'
        (_          , VFlex m' sp' ) -> solve m' sp' t'
        _                            -> throw $ UnifyError "rigid mismatch error"

unifyM :: (MonadReader r m, HasMetaCtx r, HasEnv r, MonadCatch m, MonadIO m)
    => m Val -> m Val -> m ()
unifyM t u = t >>= \t' -> u >>= \u' -> unify t' u'
