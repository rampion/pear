module Pear.Tree.Indexed where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Pear.Lens
import Pear.Opt
import Pear.Pair
import Pear.Positive
import Pear.Positive.Kind

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

reverse :: Tree n a -> Tree n a
reverse = undefined

indexes :: Tree n a -> Tree n (Fin n, a)
indexes = undefined

filter :: (a -> Bool) -> Tree n a -> Maybe (SomeTree a)
filter = undefined

mapMaybe :: (a -> Maybe b) -> Tree n a -> Maybe (SomeTree b)
mapMaybe = undefined

size :: Tree n a -> Sing n
size = undefined

head :: Tree n a -> a
head = undefined

last :: Tree n a -> a
last = undefined

(!!) :: Tree n a -> Fin n -> a
(!!) = undefined
infix 9 !!

put :: Fin n -> a -> Tree n a -> Tree n a
put = undefined

modify :: Fin n -> (a -> a) -> Tree n a -> Tree n a
modify = undefined

at :: Fin n -> Lens' a (Tree n a)
at _ = undefined

ats :: Tree n a -> Tree n (ALens' a (Tree n a))
ats = undefined

singleton :: a -> Tree 'ObI a
singleton = Top

push :: a -> Tree n a -> Tree (Succ n) a
push = undefined

pop :: Tree (Succ n) a -> (Tree n a, a)
pop = undefined

append :: Tree n a -> Tree m a -> Tree (Plus n m) a
append = undefined

split :: Tree (Plus n m) a -> (Tree n a, Tree m a)
split = undefined

fuse :: Tree n a -> Tree m a -> Tree (Plus n m) a
fuse = undefined

fiss :: Tree (Plus n m) a -> (Tree n a, Tree m a)
fiss = undefined

toNonEmpty :: Tree n a -> NonEmpty a
toNonEmpty = undefined

toList :: Tree n a -> [a]
toList = undefined

generate :: Sing n -> (Fin n -> a) -> Tree n a
generate = undefined

replicate :: Sing n -> a -> Tree n a
replicate = undefined

type SomeTree :: Type -> Type
data SomeTree a where
  SomeTree :: Tree m a -> SomeTree a

instance Eq a => Eq (SomeTree a) where
  (==) = undefined

deriving instance Show a => Show (SomeTree a)
deriving instance Functor SomeTree
deriving instance Foldable SomeTree
deriving instance Traversable SomeTree

fromNonEmpty :: NonEmpty a -> SomeTree a
fromNonEmpty = undefined

fromList :: [a] -> Maybe (SomeTree a)
fromList = undefined

withSomeTree :: (forall n. Tree n a -> r) -> SomeTree a -> r
withSomeTree _ = undefined

type Tree' :: Positive -> Type -> Type
data Tree' n a

type Zipper :: Positive -> Type -> Type
data Zipper n a

focus :: Lens' a (Zipper n a)
focus = undefined

zipUp :: Zipper n a -> Tree n a
zipUp = undefined

zipDown :: Tree n a -> Tree n (Zipper n a)
zipDown = undefined

zipNext :: Zipper n a -> Maybe (Zipper n a)
zipNext = undefined

zipPrevious :: Zipper n a -> Maybe (Zipper n a)
zipPrevious = undefined
