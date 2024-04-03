{-# LANGUAGE TemplateHaskell #-}

module Meta (
    MetaEntry(..),
    metaId,
    metaEntry,
    MetaVar(..),
    HasMetaCtx(..),
    newMetaVar,
    readMetaVar,
    readMetaVarTy,
    readUnsolvedMetaVar,
    writeMetaEntry
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

-- | Meta variable.
data MetaVar = MetaVar
    { _metaId    :: Int
    , _metaEntry :: IORef MetaEntry
    }
    deriving (Eq)

makeLenses ''MetaVar

instance Show MetaVar where
    show (MetaVar i _) = "?" ++ show i

class HasMetaCtx r where
    nextMetaId_ :: Lens' r (IORef Int)

newMetaVar :: (MonadReader r m, HasMetaCtx r, MonadIO m) => ValTy -> m MetaVar
newMetaVar a = do
    ref <- view nextMetaId_
    liftIO $ do
        mid <- readIORef ref
        writeIORef ref (mid + 1)
        MetaVar mid <$> newIORef (Unsolved a)

readMetaVar :: MonadIO m => MetaVar -> m MetaEntry
readMetaVar = liftIO . readIORef . view metaEntry

readMetaVarTy :: MonadIO m => MetaVar -> m ValTy
readMetaVarTy mvar = readMetaVar mvar <&> \case
    Solved _ a -> a
    Unsolved a -> a

readUnsolvedMetaVar :: MonadIO m => MetaVar -> m MetaEntry
readUnsolvedMetaVar mvar = do
    readMetaVar mvar >>= \case
        me@(Unsolved _) -> return me
        _ -> error "readUnsolvedMetaEntry: not an unsolved meta entry"

writeMetaEntry :: MonadIO m => MetaVar -> MetaEntry -> m ()
writeMetaEntry mvar = liftIO . writeIORef (mvar ^. metaEntry)
