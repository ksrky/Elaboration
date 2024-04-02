{-# LANGUAGE TemplateHaskell #-}

module Unify (
    UnifyError(..),
    unify
    ) where

import           Control.Exception.Safe
import           Control.Lens.Combinators
import           Control.Lens.Cons
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import qualified Data.IntMap.Strict       as IM
import           Eval
import           Meta
import           Syntax
import           Value

-- | Unification error
newtype UnifyError = UnifyError String
    deriving (Show)

instance Exception UnifyError

data PartialRenaming = ParRen {
    _occvar   :: Maybe MetaVar,
    _domain   :: Lvl,
    _codomain :: Lvl,
    _renaming :: IM.IntMap Lvl
    }
    deriving (Eq, Show)

makeLenses ''PartialRenaming

initParRen :: PartialRenaming
initParRen = ParRen
    { _occvar = Nothing
    , _domain = 0
    , _codomain = 0
    , _renaming = IM.empty
    }

liftParRen :: PartialRenaming -> PartialRenaming
liftParRen pren@(ParRen{_domain = dom, _codomain = cod}) = pren
    & domain %~ (+ 1)
    & codomain %~ (+ 1)
    & renaming %~ IM.insert cod dom

skipParRen :: PartialRenaming -> PartialRenaming
skipParRen pren = pren & codomain %~ (+ 1)

invert :: (MonadIO m, MonadThrow m) => Lvl -> Spine -> m (PartialRenaming, Pruning)
invert lvl sp = do
    (dom, ren, nlvars, fsp) <- foldrM (\(t, icit) (dom, ren, nlvars, fsp) -> do
        force t >>= \case
            VVar x | IM.member x ren ->
                return (dom + 1, IM.insert x dom ren, x : nlvars, (x, icit) : fsp)
            VVar x ->
                return (dom + 1, IM.insert x dom ren, nlvars, (x, icit) : fsp)
            _ -> throw $ UnifyError ""
        ) (0, IM.empty, [], []) sp
    let pren = map (\case
            (x, _) | x `elem` nlvars -> Nothing
            (_, i) -> Just i) fsp
    return
        ( ParRen
            { _occvar = Nothing
            , _domain = dom
            , _codomain = lvl
            , _renaming = ren
            }
        , pren)

pruneTy :: forall r m. (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) =>
    Pruning -> ValTy -> m Type
pruneTy prun ty = do
    ty' <- force ty
    go initParRen prun ty'
  where
    go :: PartialRenaming -> Pruning -> Val -> m Type
    go pren [] a = rename pren a
    go pren (pr :> Nothing) (VPi x i a b) = do
        cl <- b |@ VVar (pren ^. codomain)
        Pi x i <$> rename pren a <*> go (liftParRen pren) pr cl
    go pren (pr :> Just _) (VPi _ _ _ b) = go (skipParRen pren) pr =<< (b |@ VVar (pren ^. codomain))
    go _ _ _ = error "impossible"

pruneMeta :: (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) =>
    Pruning -> MetaVar -> m MetaVar
pruneMeta prun m = do
    mty <- readMetaEntry m >>= \case
        Unsolved a -> return a
        _          -> error "impossible"
    prunedTy <- evalClosedTerm =<< pruneTy prun mty
    m' <- newMetaVar prunedTy
    solution <- evalClosedTerm undefined -- mkLams ( length pruning) mty $ AppPruning (Meta m') pruning
    writeMetaEntry m (Solved solution mty)
    pure m'

data SpinePruneStatus
    = OKRenaming
    | OKNonRenaming
    | NeedsPruning
    deriving (Eq, Show)

pruneVFlex :: forall r m. (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) =>
    PartialRenaming -> MetaVar -> Spine -> m Term
pruneVFlex pren m sp = do
    (sp', status) <- (`runStateT` OKRenaming) $ forM sp $ \(t, i) -> do
        status <- get
        force t >>= \case
            VVar x -> case IM.lookup x (pren ^. renaming) of
                Just y -> return (Just (Var (lvl2Ix (pren ^. domain) y)), i)
                Nothing | status == OKNonRenaming -> throw $ UnifyError "non renaming"
                        | otherwise -> do
                            put NeedsPruning
                            return (Nothing, i)
            t' -> case status of
                NeedsPruning -> throw $ UnifyError "pruning needed"
                _ -> do
                    t'' <- rename pren t'
                    put OKNonRenaming
                    return (Just t'', i)
    m' <- case status of
        OKRenaming    -> readMetaEntry m >>= \case Unsolved _ -> return m; _ -> error "impossible"
        OKNonRenaming -> readMetaEntry m >>= \case Unsolved _ -> return m; _ -> error "impossible"
        NeedsPruning  -> pruneMeta (map (\(mt, i) -> i <$ mt) sp') m
    return $ foldr (\(mu, i) t -> maybe t (\u -> App t u i) mu) (Meta m') sp'

renameSpine :: (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) =>
    PartialRenaming -> Term -> Spine -> m Term
renameSpine _ t SpNil = return t
renameSpine pren t (sp :> (u, icit)) =
    App <$> renameSpine pren t sp <*> rename pren u <*> pure icit

rename :: (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) => PartialRenaming -> Val -> m Term
rename pren t = force t >>= \case
    VFlex m sp | pren ^. occvar == Just m -> throw $ UnifyError "occurs check"
               | otherwise -> pruneVFlex pren m sp
    VRigid x sp -> case IM.lookup x (pren ^. renaming) of
        Nothing -> throw $ UnifyError "scope error"
        Just x' -> renameSpine pren (Var (lvl2Ix (pren ^. domain) x')) sp
    VLam x icit c -> Lam x icit <$> (rename (liftParRen pren) =<< c |@ VVar (pren ^. codomain))
    VPi x icit a c -> Pi x icit
        <$> rename pren a
        <*> (rename (liftParRen pren) =<< c |@ VVar (pren ^. codomain))
    VU -> return U

mkLams :: forall m. MonadIO m => Lvl -> ValTy -> Term -> m Term
mkLams 0 _ t = return t
mkLams l a t = go a 0
  where
    go :: ValTy -> Lvl -> m Term
    go _ l' | l' == l = return t
    go a' l' = force a' >>= \case
        VPi "_" i _ b -> do
            cl <- b |@ VVar l'
            Lam ("x" ++ show l') i <$> go cl (l' + 1)
        VPi x i _ b   -> do
            cl <- b |@ VVar l'
            Lam x i <$> go cl (l' + 1)
        _ -> error "impossible"

solve :: (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) => Lvl -> MetaVar -> Spine -> Val -> m ()
solve lvl mvar sp rhs = do
    prenprun <- invert lvl sp
    solveWithParRen mvar prenprun rhs

solveWithParRen :: (MonadReader r m, HasMetaCtx r, MonadIO m, MonadThrow m) =>
    MetaVar -> (PartialRenaming, Pruning) -> Val -> m ()
solveWithParRen mvar (pren, prun) rhs = do
    mty <- readMetaEntry mvar >>= \case
        Unsolved a -> return a
        _          -> error "impossible"
    _ <- pruneTy prun mty
    rhs' <- rename pren{_occvar = Just mvar} rhs
    solution <- evalClosedTerm =<< mkLams (pren ^. domain) mty rhs'
    writeMetaEntry mvar (Solved solution mty)

-- | Unify spines.
unifySpine :: (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m)
    => Lvl -> Spine -> Spine -> m ()
unifySpine _ SpNil SpNil           = return ()
unifySpine l (sp :> (t, _)) (sp' :> (t', _)) = unifySpine l sp sp' >> unify l t t'
unifySpine _ _ _                   = throw $ UnifyError "rigid mismatch error"

-- | Unify values.
unify :: (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m)
    => Lvl -> Val -> Val -> m ()
unify l t u = do
    t' <- force t
    u' <- force u
    case (t', u') of
        (VLam _ _ c , VLam _ _ c') -> unifyM (l + 1) (c |@ VVar l) (c' |@ VVar l)
        (_        , VLam _ i c') -> unifyM (l + 1) (vApp t' (VVar l) i) (c' |@ VVar l)
        (VLam _ icit c , _        ) -> unifyM (l + 1) (c |@ VVar l) (vApp t' (VVar l) icit)
        (VU       , VU       ) -> return ()
        (VPi _ i a c, VPi _ i' a' c') | i == i' -> do
            unify l a a'
            unifyM (l + 1) (c |@ VVar l) (c' |@ VVar l)
        (VRigid x sp, VRigid x' sp') | x == x' -> unifySpine l sp sp'
        (VFlex m sp , VFlex m' sp') | m == m' -> intersect l m sp sp'
        (VFlex m sp , VFlex m' sp')           -> flexFlex l m sp m' sp'
        (VFlex m sp , _            ) -> solve l m sp u'
        (_          , VFlex m' sp' ) -> solve l m' sp' t'
        _                            -> throw $ UnifyError "rigid mismatch error"

flexFlex :: (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m) =>
    Lvl -> MetaVar -> Spine -> MetaVar -> Spine -> m ()
flexFlex gamma m sp m' sp'
    | spineLength sp < spineLength sp' = flexFlex gamma m' sp' m sp
    | otherwise = (do
        pren <- invert gamma sp
        solveWithParRen m pren (VFlex m' sp')
        ) `catch` \(UnifyError _) -> solve gamma m' sp' (VFlex m sp)

intersect :: forall r m. (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m) =>
    Lvl -> MetaVar -> Spine -> Spine -> m ()
intersect l m sp sp' =
    go sp sp' >>= \case
        Nothing                     -> unifySpine l sp sp'
        Just pr | Nothing `elem` pr -> void $ pruneMeta pr m
                | otherwise         -> pure ()
  where
    go :: Spine -> Spine -> m (Maybe Pruning)
    go SpNil SpNil = return $ Just []
    go (sp0 :> (t, i)) (sp0' :> (t', _)) = do
        u <- force t
        u' <- force t'
        case (u, u') of
            (VVar x, VVar x') | x == x' -> do
                mb_pr <- go sp0 sp0'
                return $ (Just i:) <$> mb_pr
            _ -> return Nothing
    go _ _ = error "impossible"

unifyM :: (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m)
    => Lvl -> m Val -> m Val -> m ()
unifyM l mt mu = mt >>= \t -> mu >>= \u -> unify l t u
