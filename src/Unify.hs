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
import           Data.Foldable
import qualified Data.IntMap.Strict       as IM
import           Eval
import           Meta
import           Syntax
import           Value
import qualified Value.Env                as Env

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

liftParRen :: PartialRenaming -> PartialRenaming
liftParRen pren@(ParRen{_domain = dom, _codomain = cod}) = pren
    & domain %~ (+ 1)
    & codomain %~ (+ 1)
    & renaming %~ IM.insert cod dom

skipParRen :: PartialRenaming -> PartialRenaming
skipParRen pren = pren & codomain %~ (+ 1)

invert :: (MonadIO m, MonadThrow m) => Lvl -> Spine -> m PartialRenaming
invert lvl sp = do
    (dom, ren, nlvars, fsp) <- foldrM (\(t, icit) (dom, ren, nlvars, fsp) -> do
        force t >>= \case
            VVar x | IM.notMember x ren || x `elem` nlvars ->
                return (dom + 1, IM.insert x dom ren, x : nlvars, (x, icit) : fsp)
            VVar x ->
                return (dom + 1, IM.insert x dom ren, nlvars, (x, icit) : fsp)
            _ -> throw $ UnifyError ""
        ) (0, IM.empty, [], []) (unSpine sp)
    let pr = map (\case
            (x, _) | x `elem` nlvars -> Nothing
            (_, i) -> Just i) fsp
        mb_pr = if null pr then Nothing else Just pr
    return $ ParRen
        { _occvar = Nothing
        , _domain = dom
        , _codomain = lvl
        , _renaming = ren
        }

renameSpine :: (MonadIO m, MonadThrow m) => PartialRenaming -> Term -> Spine -> m Term
renameSpine _ t SpNil = return t
renameSpine pren t (sp :> (u, icit)) =
    App <$> renameSpine pren t sp <*> rename pren u <*> pure icit

rename :: (MonadIO m, MonadThrow m) => PartialRenaming -> Val -> m Term
rename pren t = force t >>= \case
    VFlex m sp | pren ^. occvar == Just m -> throw $ UnifyError ""
               | otherwise -> renameSpine pren (Meta m) sp
    VRigid x sp -> case IM.lookup x (pren ^. renaming) of
        Nothing -> throw $ UnifyError ""
        Just x' -> renameSpine pren (Var (lvl2Ix (pren ^. domain) x')) sp
    VLam x icit c -> Lam x icit <$> (rename (liftParRen pren) =<< c |@ VVar (pren ^. codomain))
    VPi x icit a c -> Pi x icit
        <$> rename pren a
        <*> (rename (liftParRen pren) =<< c |@ VVar (pren ^. codomain))
    VU -> return U

mkLams :: Lvl -> Term -> Term
mkLams 0 t = t
mkLams l t = undefined -- Lam ("x" ++ show l) (mkLams (l - 1) t)

solve :: (MonadIO m, MonadThrow m) => Lvl -> MetaVar -> Spine -> Val -> m ()
solve lvl mvar sp rhs = do
    pren <- invert lvl sp
    rhs' <- rename pren{_occvar = Just mvar} rhs
    solution <- evalTerm Env.empty $ mkLams (pren ^. domain) rhs'
    writeMetaEntry mvar (Solved solution)

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
        (_        , VLam _ icit c') -> unifyM (l + 1) (vApp t' (VVar l) icit) (c' |@ VVar l)
        (VLam _ icit c , _        ) -> unifyM (l + 1) (c |@ VVar l) (vApp t' (VVar l) icit)
        (VU       , VU       ) -> return ()
        (VPi _ _ a c, VPi _ _ a' c') -> do
            unify l a a'
            unifyM (l + 1) (c |@ VVar l) (c' |@ VVar l)
        (VRigid x sp, VRigid x' sp') | x == x' -> unifySpine l sp sp'
        (VFlex m sp , VFlex m' sp' ) | m == m' -> unifySpine l sp sp'
        (VFlex m sp , _            ) -> solve l m sp u'
        (_          , VFlex m' sp' ) -> solve l m' sp' t'
        _                            -> throw $ UnifyError "rigid mismatch error"

unifyM :: (MonadReader r m, HasMetaCtx r, MonadCatch m, MonadIO m)
    => Lvl -> m Val -> m Val -> m ()
unifyM l mt mu = mt >>= \t -> mu >>= \u -> unify l t u
