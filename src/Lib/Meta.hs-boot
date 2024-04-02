module Lib.Meta (MEntry, MVar) where

import Data.Eq
import Data.Int
import Data.IORef
import GHC.Show

data MEntry

data MVar = MVar {_mid :: Int, _mentry :: IORef MEntry}

instance Eq MVar
instance Show MVar
