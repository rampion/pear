-- | Convenience types for working with injections, surjections and bijections
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Base₂.Function where

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

instance Category (Inverse i s) => Category (Function i s) where
  id = Function
    { image = id
    , preimage = runInverse @i @s id
    }
  f . g = Function
    { image = image f . image g
    , preimage = runInverse (inverse f . inverse g)
    }

inverse :: Function i s a b -> Inverse i s a b
inverse = Inverse . preimage

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

type Preimage :: Injectivity -> Surjectivity -> Type -> Type
type family Preimage i s a where
  Preimage 'Noninjective 'Nonsurjective a = [a]
  Preimage 'Noninjective 'Surjective a = NonEmpty a
  Preimage 'Injective 'Nonsurjective a = Maybe a
  Preimage 'Injective 'Surjective a = a

type Inverse :: Injectivity -> Surjectivity -> Type -> Type -> Type
newtype Inverse i s a b = Inverse { runInverse :: b -> Preimage i s a }

instance Category (Inverse 'Noninjective 'Nonsurjective) where
  id = Inverse pure
  Inverse f . Inverse g = Inverse (f >=> g)

instance Category (Inverse 'Noninjective 'Surjective) where
  id = Inverse pure
  Inverse f . Inverse g = Inverse (f >=> g)

instance Category (Inverse 'Injective 'Nonsurjective) where
  id = Inverse pure
  Inverse f . Inverse g = Inverse (f >=> g)

instance Category (Inverse 'Injective 'Surjective) where
  id = Inverse id
  Inverse f . Inverse g = Inverse (f >>> g)


to :: Function i s a b -> a -> b
to = image

from :: Function i s a b -> b -> Preimage i s a
from = preimage

type (*->) :: Type -> Type -> Type
type (*->) = Function 'Noninjective 'Nonsurjective

type (+->) :: Type -> Type -> Type
type (+->) = Function 'Noninjective 'Surjective

type (?->) :: Type -> Type -> Type
type (?->) = Function 'Injective 'Nonsurjective

type (<->) :: Type -> Type -> Type
type (<->) = Function 'Injective 'Surjective
