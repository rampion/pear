-- | Convenience types for working with injections, surjections and bijections
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Pear.Function where

import Prelude hiding ((.), id)
import GHC.Types (Type)
import Control.Category (Category(..), (>>>))
import Data.List.NonEmpty (NonEmpty)
import Control.Monad ((>=>))

-- | Specification of a function along with its preimage
--
-- Legal values should satisfy these two properties:
--
-- Give f :: Function I S A B, then
--
--  1. ∀ a :: A, a ∈ preimage f (image a)
--  2. ∀ b :: B, ∀ a ∈ preimage f b, b == image f a
type Function :: Injectivity -> Surjectivity -> Type -> Type -> Type
data Function i s a b = Function
  { image :: a -> b                 -- ^ run the function forward
  , preimage :: b -> Preimage i s a -- ^ run the function backward
  }

instance Category (InverseImage i s) => Category (Function i s) where
  id = Function
    { image = id
    , preimage = runInverseImage @i @s id
    }
  f . g = Function
    { image = image f . image g
    , preimage = runInverseImage (inverseImage f . inverseImage g)
    }

-- | Extact the computation of the inverse image from a function.
inverseImage :: Function i s a b -> InverseImage i s a b
inverseImage = InverseImage . preimage

-- | Flag for whether a function is injective, aka "1 to 1"
--
-- f :: A -> B is __injective__ if and only if for all a₀, a₁ :: A,
-- f a₀ = f a₁ ⇒ a₀ = a₁
type Injectivity :: Type
data Injectivity = Noninjective | Injective

-- | Flag for whether a function is surjective, aka "onto"
--
-- f :: A -> B is __surjective__ if and only if for all b :: B,
-- there exists a :: A such that f a = b
type Surjectivity :: Type
data Surjectivity = Nonsurjective | Surjective

-- | Shape of the preimage of a function; how
--   many elements in the domain can map to a single
--   element in the codomain
type Preimage :: Injectivity -> Surjectivity -> Type -> Type
type family Preimage i s a where
  Preimage 'Noninjective 'Nonsurjective a = [a]       -- general case: no limit on the size of the preimage
  Preimage 'Noninjective 'Surjective a = NonEmpty a   -- surjective : ∀ b, ∃ at least one a s.t f a = b
  Preimage 'Injective 'Nonsurjective a = Maybe a      -- injective : ∀ b, ∃ at most one a s.t. f a = b
  Preimage 'Injective 'Surjective a = a               -- bijective : ∀ b, ∃ exactly one a s.t. f a = b

-- | Inverse image of a function
type InverseImage :: Injectivity -> Surjectivity -> Type -> Type -> Type
newtype InverseImage i s a b = InverseImage { runInverseImage :: b -> Preimage i s a }

instance Category (InverseImage 'Noninjective 'Nonsurjective) where
  id = InverseImage pure
  InverseImage f . InverseImage g = InverseImage (f >=> g)

instance Category (InverseImage 'Noninjective 'Surjective) where
  id = InverseImage pure
  InverseImage f . InverseImage g = InverseImage (f >=> g)

instance Category (InverseImage 'Injective 'Nonsurjective) where
  id = InverseImage pure
  InverseImage f . InverseImage g = InverseImage (f >=> g)

instance Category (InverseImage 'Injective 'Surjective) where
  id = InverseImage id
  InverseImage f . InverseImage g = InverseImage (f >>> g)


-- | run a function forwards
to :: Function i s a b -> a -> b
to = image

-- | run a function backwards
from :: Function i s a b -> b -> Preimage i s a
from = preimage

-- | general function
type (*->) :: Type -> Type -> Type
type (*->) = Function 'Noninjective 'Nonsurjective

-- | surjective function
type (+->) :: Type -> Type -> Type
type (+->) = Function 'Noninjective 'Surjective

-- | injective function
type (?->) :: Type -> Type -> Type
type (?->) = Function 'Injective 'Nonsurjective

-- | bijective function
type (<->) :: Type -> Type -> Type
type (<->) = Function 'Injective 'Surjective
