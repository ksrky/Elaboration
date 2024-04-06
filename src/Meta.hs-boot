module Meta (MetaEntry, MetaVar, UnsolvedMetaVar) where

data MetaEntry

data MetaVar

instance Eq MetaVar
instance Show MetaVar

data UnsolvedMetaVar

instance Eq UnsolvedMetaVar
instance Show UnsolvedMetaVar
