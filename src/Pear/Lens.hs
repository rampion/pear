-- | Useful Van Laarhoven-style optics
module Pear.Lens where

import Control.Applicative (Alternative)
import Data.Kind (Type)

-- | The classic type-invariant lens
type Lens' :: Type -> Type -> Type
type Lens' a t = forall f. Functor f => (a -> f a) -> t -> f t

get :: Lens' a t -> t -> a
get _ = undefined

set :: Lens' a t -> a -> t -> t
set _ = undefined

over :: Lens' a t -> (a -> a) -> t -> t
over _ = undefined

newtype ALens' a t = ALens' { fromALens' :: Lens' a t }

-- | A lens with the possibility of failure, useful for list-like collectios
type Shard :: Type -> Type -> Type
type Shard a t = forall f. Alternative f => (a -> f a) -> f t

getShard :: Shard a t -> t -> Maybe a
getShard _ = undefined

setShard :: Shard a t -> a -> t -> Maybe t
setShard _ = undefined

overShard :: Shard a t -> (a -> a) -> t -> Maybe t
overShard _ = undefined

-- | A pseudo-optic that has already been bound to target
type Focused :: Type -> Type -> Type
newtype Focused a t = Focused { withFocused :: forall f. Functor f => (a -> f a) -> f t }

getFocused :: Focused a t -> a
getFocused = undefined

setFocused :: Focused a t -> a -> t
setFocused = undefined

overFocused :: Focused a t -> (a -> a) -> t
overFocused = undefined
