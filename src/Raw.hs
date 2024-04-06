{-# LANGUAGE TemplateHaskell #-}

module Raw
    ( ArgInfo
    , SrcPos
    , Raw(..)
    , RawF(..)
    , stripSrcPos
    ) where

import Common
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- | Argument information
type ArgInfo = Either Name Icit

-- | Source position
type SrcPos = (Int, Int)

data Raw
    = -- | @x@
      Var Name
    | -- | @λx → t@ | @λ{x} → t@ | @λ{l = x} → t@
      Lam Name ArgInfo Raw
    | -- | @t u@ | @t {u}@ | @t {x = u}@
      App Raw Raw ArgInfo
    | -- | @(x : a) → b@ | @{x : a} → b@
      Pi Name Icit Raw Raw
    | -- | @let x : a = t; u@
      Let Name Raw Raw Raw
    | -- | @U@
      U
    | -- | @_@
      Hole
      -- | @t@ with source position
    | SrcPos SrcPos Raw
    deriving (Eq, Show)

makeBaseFunctor ''Raw

-- | Strip source position info from Raw syntax.
stripSrcPos :: Raw -> Raw
stripSrcPos = cata $ \case
    SrcPosF _ r -> stripSrcPos r
    r          -> embed r
