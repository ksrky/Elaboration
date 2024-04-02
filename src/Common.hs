module Common (Name, Icit(..)) where
import           Control.Lens.Cons
import           Control.Lens.Prism

-- | Variable name.
type Name = String

-- | Explicit or implicit.
data Icit = Expl | Impl
    deriving (Eq, Show)

newtype SnocList a = SnocList [a]

instance Cons (SnocList a) (SnocList a) a a where
    _Cons = prism
        (\(x, SnocList xs) -> SnocList (xs ++ [x]))
        (\(SnocList xs) -> case unsnoc xs of
            Nothing       -> Left (SnocList [])
            Just (xs', x) -> Right (x, SnocList xs'))

instance Snoc (SnocList a) (SnocList a) a a where
    _Snoc = prism
        (\(SnocList xs, x) -> SnocList (x : xs))
        (\(SnocList xs) -> case xs of
            []        -> Left (SnocList [])
            (x : xs') -> Right (SnocList xs', x))
