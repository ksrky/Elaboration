{-# LANGUAGE TemplateHaskell #-}

module Raw (
    SrcPos,
    Raw(..),
    RawF(..),
    stripSrcPos
    ) where

import           Common
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH

type SrcPos = (Int, Int)

data Raw
    = Var Name
    | Lam Name Raw
    | App Raw Raw
    | Pi Name Raw Raw
    | Let Name Raw Raw Raw
    | U
    | Hole
    | SrcPos SrcPos Raw
    deriving (Eq, Show)

makeBaseFunctor ''Raw

stripSrcPos :: Raw -> Raw
stripSrcPos = cata $ \case
    SrcPosF _ r -> stripSrcPos r
    r          -> embed r
