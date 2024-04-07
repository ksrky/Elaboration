module Meta (MetaEntry, MetaVar, UnsolvedMetaVar) where

data MetaEntry

data MetaVar

instance Eq MetaVar
instance Show MetaVar

type UnsolvedMetaVar = MetaVar
