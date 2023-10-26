module Pear.Selected where

import Data.Kind (Type)

-- | a hypolens
type (:←) :: Type -> Type -> Type
newtype (:←) a t = Selected { withSelected :: forall f. Functor f => (a -> f a) -> f t }

get :: a :← t -> a
get = undefined

set :: a :← t -> a -> t
set = undefined
