module Pear.Tree where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Pear.Lens
import Pear.Pair
import Pear.Positive

type Tree :: Type -> Type
data Tree a = Top a | Tree (Pair a) :>- Maybe a

deriving instance Eq a => Eq (Tree a)
deriving instance Functor Tree
deriving instance Foldable Tree
deriving instance Traversable Tree

instance Show a => Show (Tree a) where
  showsPrec = undefined

instance Semigroup (Tree a) where
  (<>) = undefined

type Tree0 :: Type -> Type
type Tree0 a = Maybe (Tree a)

reverse :: Tree a -> Tree a
reverse = undefined

reverse0 :: Tree0 a -> Tree0 a
reverse0 = undefined

focus :: Natural -> Tree a -> Maybe (Focused a (Tree a))
focus = undefined

at :: Natural -> Shard a (Tree a)
at _ = undefined

indexes :: Tree a -> Tree (Natural, a)
indexes = undefined

foci :: Tree a -> Tree (Focused a (Tree a))
foci = undefined

filter :: (a -> Bool) -> Tree a -> Tree0 a
filter = undefined

mapMaybe :: (a -> Maybe b) -> Tree a -> Tree0 b
mapMaybe = undefined

push :: a -> Tree a -> Tree a
push = undefined

push0 :: a -> Tree0 a -> Tree a
push0 = undefined

pop :: Tree a -> (Tree0 a, a)
pop = undefined

pop2 :: Tree (Pair a) -> (Tree a, a)
pop2 = undefined

size :: Tree a -> Positive
size = undefined

size0 :: Tree0 a -> Natural
size0 = undefined

split :: Positive -> Tree a -> Maybe (Tree a, Tree a)
split = undefined

split0 :: Natural -> Tree a -> (Tree0 a, Tree0 a)
split0 = undefined

fuse :: Tree a -> Tree a -> Tree a
fuse = undefined

fuse0 :: Tree0 a -> Tree0 a -> Tree0 a
fuse0 = undefined

fiss :: Positive -> Tree a -> Maybe (Tree a, Tree a)
fiss = undefined

fiss0 :: Natural -> Tree a -> (Tree0 a, Tree0 a)
fiss0 = undefined

fromNonEmpty :: NonEmpty a -> Tree a
fromNonEmpty = undefined

fromList :: [a] -> Tree0 a
fromList = undefined

toNonEmpty :: Tree a -> NonEmpty a
toNonEmpty = undefined

toList :: Tree0 a -> [a]
toList = undefined

generate :: Positive -> (Natural -> a) -> Tree a
generate = undefined

generate0 :: Natural -> (Natural -> a) -> Tree0 a
generate0 = undefined

replicate :: Positive -> a -> Tree a
replicate = undefined

replicate0 :: Natural -> a -> Tree0 a
replicate0 = undefined
