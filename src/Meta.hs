{-# LANGUAGE TemplateHaskell #-}

module Meta
    ( MetaEntry(..)
    , metaId
    , metaEntry
    , MetaVar(..)
    , UnsolvedMetaVar
    , HasMetaCtx(..)
    , newMetaVar
    , readMetaVar
    , readMetaVarTy
    , readUnsolvedMetaVar
    , writeMetaEntry
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.IORef
import Value

-- | Meta entry.
data MetaEntry
    = Solved Val ValTy
    | Unsolved ValTy
    deriving (Eq, Show)

-- | Meta id.
type MetaId = Int

-- | Meta variable.
data MetaVar = MetaVar
    { _metaId    :: MetaId
    , _metaEntry :: IORef MetaEntry
    } deriving (Eq)

makeLenses ''MetaVar

instance Show MetaVar where
    show (MetaVar i _) = "?" ++ show i

type UnsolvedMetaVar = MetaVar

class HasMetaCtx r where
    nextMetaId_ :: Lens' r (IORef MetaId)

newMetaVar :: (MonadReader r m, HasMetaCtx r, MonadIO m) => ValTy -> m UnsolvedMetaVar
newMetaVar a = do
    ref <- view nextMetaId_
    liftIO $ do
        mid <- readIORef ref
        writeIORef ref (mid + 1)
        MetaVar mid <$> newIORef (Unsolved a)

readMetaVar :: MonadIO m => MetaVar -> m MetaEntry
readMetaVar = liftIO . readIORef . view metaEntry

readMetaVarTy :: MonadIO m => MetaVar -> m ValTy
readMetaVarTy m = readMetaVar m <&> \case
    Solved _ a -> a
    Unsolved a -> a

readUnsolvedMetaVar :: MonadIO m => UnsolvedMetaVar -> m MetaEntry
readUnsolvedMetaVar m = do
    readMetaVar m >>= \case
        me@(Unsolved _) -> return me
        _ -> error "readUnsolvedMetaEntry: not an unsolved meta entry"

writeMetaEntry :: MonadIO m => MetaVar -> MetaEntry -> m ()
writeMetaEntry mvar = liftIO . writeIORef (mvar ^. metaEntry)
