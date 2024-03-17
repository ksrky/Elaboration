{-# LANGUAGE TemplateHaskell #-}

module Syntax (Ix, Lvl, Type, Term(..), TermF(..)) where

import           Common
import           Data.Functor.Foldable.TH

-- | De Bruijn index.
type Ix = Int

-- | De Bruijn level.
type Lvl = Int

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
    deriving (Eq, Show)

makeBaseFunctor ''Term
