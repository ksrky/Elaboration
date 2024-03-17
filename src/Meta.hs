{-# LANGUAGE TemplateHaskell #-}

module Meta (
    MetaEntry(..),
    metaId,
    metaEntry,
    MetaVar(..),
    HasMetaCtx(..),
    newMetaVar,
    readMetaEntry,
    writeMetaEntry
    ) where

import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.IORef
import           Value

-- | Meta entry.
data MetaEntry = Solved Val | Unsolved
    deriving (Eq, Show)

-- | Meta variable.
data MetaVar = MetaVar {_metaId :: Int, _metaEntry :: IORef MetaEntry}
    deriving (Eq)

makeLenses ''MetaVar

instance Show MetaVar where
    show (MetaVar i _) = "?" ++ show i

class HasMetaCtx r where
    nextMetaId_ :: Lens' r (IORef Int)

newMetaVar :: (MonadReader r m, HasMetaCtx r, MonadIO m) => m MetaVar
newMetaVar = do
    ref <- view nextMetaId_
    liftIO $ do
        mid <- readIORef ref
        writeIORef ref (mid + 1)
        MetaVar mid <$> newIORef Unsolved

readMetaEntry :: MonadIO m => MetaVar -> m MetaEntry
readMetaEntry = liftIO . readIORef . view metaEntry

writeMetaEntry :: MonadIO m => MetaVar -> MetaEntry -> m ()
writeMetaEntry mvar = liftIO . writeIORef (mvar ^. metaEntry)
