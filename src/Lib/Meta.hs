module Lib.Meta (
    MEntry(..),
    mentry,
    MVar(..),
    MetaCtx,
    HasMetaCtx(..),
    newMVar,
    readMEntry,
    writeMEntry) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.IORef
import GHC.Base
import GHC.Num
import GHC.Show
import Lib.Value

-- | Meta entry.
data MEntry = Solved Val | Unsolved
    deriving (Eq, Show)

-- | Meta variable.
data MVar = MVar {_mid :: Int, _mentry :: IORef MEntry}
    deriving (Eq)

instance Show MVar where
    show (MVar i _) = "?" ++ show i

mentry :: Lens' MVar (IORef MEntry)
mentry = lens _mentry (\m e -> m{_mentry = e})

newtype MetaCtx = MetaCtx {_mnext :: IORef Int}

class HasMetaCtx a where
    metaCtxL :: Lens' a MetaCtx
    mnext :: Lens' a (IORef Int)
    mnext = metaCtxL . go where go f (MetaCtx x) = MetaCtx <$> f x

instance HasMetaCtx MetaCtx where
    metaCtxL = id

newMVar :: (MonadReader r m, HasMetaCtx r, MonadIO m) => m MVar
newMVar = do
    mn <- view mnext
    i <- liftIO $ readIORef mn
    liftIO $ writeIORef mn (i + 1)
    liftIO $ MVar i <$> newIORef Unsolved

readMEntry :: MonadIO m => MVar -> m MEntry
readMEntry = liftIO . readIORef . view mentry

writeMEntry :: MonadIO m => MVar -> MEntry -> m ()
writeMEntry m = liftIO . writeIORef (m ^. mentry)
