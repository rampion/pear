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
import Control.Category
import Data.List.NonEmpty (NonEmpty)
import Control.Monad ((>=>))

type Function :: Injectivity -> Surjectivity -> * -> * -> *
data Function i s a b = Function
  { image :: a -> b
  , preimage :: b -> Preimage i s a
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

type Injectivity :: *
data Injectivity = Noninjective | Injective

type Surjectivity :: *
data Surjectivity = Nonsurjective | Surjective

type Preimage :: Injectivity -> Surjectivity -> * -> *
type family Preimage i s a where
  Preimage 'Noninjective 'Nonsurjective a = [a]
  Preimage 'Noninjective 'Surjective a = NonEmpty a
  Preimage 'Injective 'Nonsurjective a = Maybe a
  Preimage 'Injective 'Surjective a = a

type Inverse :: Injectivity -> Surjectivity -> * -> * -> *
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

type (*->) :: * -> * -> *
type (*->) = Function 'Noninjective 'Nonsurjective

type (+->) :: * -> * -> *
type (+->) = Function 'Noninjective 'Surjective

type (?->) :: * -> * -> *
type (?->) = Function 'Injective 'Nonsurjective

type (<->) :: * -> * -> *
type (<->) = Function 'Injective 'Surjective
