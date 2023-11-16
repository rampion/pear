-- | Useful Van Laarhoven-style optics
module Pear.Lens where
-- XXX Is this necessary? Only `Lens` is used in Zipper.hs and only there
--     Should be inlined instead?

import Data.Functor.Const (pattern Const, getConst)
import Data.Functor.Identity (pattern Identity, runIdentity)
import Data.Kind (Type)

-- | The classic type-invariant lens
type Lens' :: Type -> Type -> Type
type Lens' a t = forall f. Functor f => (a -> f a) -> t -> f t

get :: Lens' a t -> t -> a
get l = getConst . l Const

set :: Lens' a t -> a -> t -> t
set l = over l . const

over :: Lens' a t -> (a -> a) -> t -> t
over l f = runIdentity . l (Identity . f)

type ALens' :: Type -> Type -> Type
newtype ALens' a t = ALens' { fromALens' :: Lens' a t }
