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
import Data.Eq
import Data.Function
import Data.Int
import Data.IORef
import Data.List                  ((++))
import GHC.Num
import GHC.Show
import Lib.Value

-- | Meta entry.
data MEntry = Solved Val VTy | Unsolved VTy
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

newMVar :: (MonadReader r m, HasMetaCtx r, MonadIO m) => VTy -> m MVar
newMVar a = do
    mn <- view mnext
    liftIO $ do
        i <- readIORef mn
        writeIORef mn (i + 1)
        MVar i <$> newIORef (Unsolved a)

readMEntry :: MonadIO m => MVar -> m MEntry
readMEntry = liftIO . readIORef . view mentry

writeMEntry :: MonadIO m => MVar -> MEntry -> m ()
writeMEntry m = liftIO . writeIORef (m ^. mentry)
