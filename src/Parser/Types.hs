{-# LANGUAGE DeriveLift #-}

module Parser.Types where

import Data.List                  qualified as L
import Data.Text                  qualified as T
import Data.Vector                qualified as V
import Language.Haskell.TH.Syntax qualified as TH

-- | Parser name, which can be a node label of the syntax tree.
newtype Name = Name String
    deriving (Eq, Show)

-- | Each expression expected by the operator has a binding power.
type BindingPower = Int

-- * Token

class (Show t, Ord t) => Token t where
    tokenString :: t -> String
    tokenString = show

instance Token String where
    tokenString = id

instance Token T.Text where
    tokenString = T.unpack

-- * Operator
data OpExp t
    = Op t
    | Exp BindingPower
    deriving (TH.Lift)

instance Show t => Show (OpExp t) where
    show (Op t)   = show t
    show (Exp bp) = ':' : show bp

data MixfixOp t = MixfixOp
    { name   :: Name
    , opExps :: [OpExp t]
    }
    deriving (Show)


-- | Generalized AST.
data Syntax
    = -- | @Name@ corresponds to a data constructor of the AST
      -- and @[Syntax]@ is its field.
      Node Name [Syntax]
    | -- | Identifier or literal.
      Atom String
    deriving (Eq)

instance Show Syntax where
    show (Node (Name name) stxs) = name ++ " [" ++ L.intercalate ", " (map show stxs) ++ "]"
    show (Atom str)              = show str

type SyntaxStack = V.Vector Syntax
