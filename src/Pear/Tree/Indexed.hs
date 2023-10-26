module Pear.Tree.Indexed where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Pear.Pair
import Pear.Opt
import Pear.Positive
import Pear.Positive.Kind
import Pear.Selected

type Tree :: Positive -> Type -> Type
data Tree n a where
  Top :: a -> Tree 'ObI a
  (:>-) :: Tree bs (Pair a) -> Opt b a -> Tree (bs ':. b) a

deriving instance Eq a => Eq (Tree n a)
deriving instance Functor (Tree n)
deriving instance Foldable (Tree n)
deriving instance Traversable (Tree n)

instance Show a => Show (Tree n a) where
  showsPrec = undefined

type SomeTree :: Type -> Type
data SomeTree a where
  SomeTree :: Tree m a -> SomeTree a

instance Eq a => Eq (SomeTree a) where
  (==) = undefined

deriving instance Show a => Show (SomeTree a)
deriving instance Functor SomeTree
deriving instance Foldable SomeTree
deriving instance Traversable SomeTree


at :: Fin n -> Tree n a -> (a :← Tree n a)
at = undefined

reverse :: Tree n a -> Tree n a
reverse = undefined

indexes :: Tree n a -> Tree n (Fin n, a)
indexes = undefined

selects :: Tree n a -> Tree n (a :← Tree n a)
selects = undefined

filter :: (a -> Bool) -> Tree n a -> Maybe (SomeTree a)
filter = undefined

mapMaybe :: (a -> Maybe b) -> Tree n a -> Maybe (SomeTree b)
mapMaybe = undefined

push :: a -> Tree n a -> Tree (Succ n) a
push = undefined

pop :: Tree (Succ n) a -> (Tree n a, a)
pop = undefined

size :: Tree n a -> Sing n
size = undefined

append :: Tree n a -> Tree m a -> Tree (Plus n m) a
append = undefined

split :: Tree (Plus n m) a -> (Tree n a, Tree m a)
split = undefined

fuse :: Tree n a -> Tree m a -> Tree (Plus n m) a
fuse = undefined

fiss :: Tree (Plus n m) a -> (Tree n a, Tree m a)
fiss = undefined

fromNonEmpty :: NonEmpty a -> SomeTree a
fromNonEmpty = undefined

fromList :: [a] -> Maybe (SomeTree a)
fromList = undefined

toNonEmpty :: Tree n a -> NonEmpty a
toNonEmpty = undefined

toList :: Tree n a -> [a]
toList = undefined

generate :: Sing n -> (Fin n -> a) -> Tree n a
generate = undefined

replicate :: Sing n -> a -> Tree n a
replicate = undefined
