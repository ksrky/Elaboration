{-# LANGUAGE TemplateHaskell #-}

module Syntax (
    Ix,
    Lvl,
    EnvSpec(..),
    Type,
    Term(..),
    TermF(..)
    ) where

import                          Common
import                          Data.Functor.Foldable.TH
import {-# SOURCE #-}           Meta

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

-- | Bound or Defined
data EnvSpec = Bound | Defined
    deriving (Eq, Show)

-- | Types.
type Type = Term

-- | Terms. @Tm n@ means that the term is well-scoped
-- under the environment of length @n@
data Term
    = Var Ix
    | App Term Term
    | Lam Name Term
    | Let Name Type Term Term
    | Pi Name Type Type
    | U
    | Meta MetaVar
    | InsertedMeta MetaVar [EnvSpec]
    deriving (Eq, Show)

makeBaseFunctor ''Term
