{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Natural.Binary.Two where

import Control.Monad (ap)

infixr 8 :*

type Two :: * -> *
data Two a = !a :* !a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Two where
  pure a = a :* a
  (<*>) = ap

instance Monad Two where
  (a0 :* a1) >>= f = f a0 `diag` f a1

diag :: Two a -> Two a -> Two a
diag (a0 :* _) (_ :* a1) = a0 :* a1

fst :: Two a -> a
fst (a :* _) = a

snd :: Two a -> a
snd (_ :* a) = a
