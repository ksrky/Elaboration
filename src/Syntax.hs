{-# LANGUAGE TemplateHaskell #-}

module Syntax (
    Ix,
    Lvl,
    Pruning,
    Type,
    Term(..),
    pattern Arrow,
    TermF(..)
    ) where

import                Common
import                Data.Functor.Foldable.TH
import {-# SOURCE #-} Meta

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

-- | Pruning
-- Bound = Just icit, Defined = Nothing
type Pruning = [Maybe Icit]

-- | Types.
type Type = Term

-- | Terms.
data Term
    = Var Ix
    | App Term Term Icit
    | AppPruning Term Pruning
    | Lam Name Icit Term
    | Let Name Type Term Term
    | Pi Name Icit Type Type
    | U
    | Meta MetaVar
    deriving (Eq, Show)

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = Pi "_" Expl a b

makeBaseFunctor ''Term
