{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Base₂.Two where

import Base₂.Indexed
import Base₂.Via.Deindexed
import Base₂.Bit
import Data.Functor ((<&>))
import Control.Applicative (liftA2)

infixr 8 :*

type Two :: * -> *
data Two a = !a :* !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving (Applicative, Monad) via Deindexed Two

instance Indexed Two where
  type Ix Two = Bit
  at O f (aO :* aI) = f aO <&> \aO -> aO :* aI
  at I f (aO :* aI) = f aI <&> \aI -> aO :* aI

instance IFunctor Two where
  imap = itwo (:*)

instance IApplicative Two where
  ipure f = f O :* f I
  liftIA2 = liftIM2

instance IMonad Two where
  ibind = itwo diag

instance IFoldable Two where
  ifoldMap = itwo (<>)

instance ITraversable Two where
  itraverse = itwo (liftA2 (:*))

itwo :: (b -> b -> c) -> (Bit -> a -> b) -> Two a -> c
itwo op f (aO :* aI) = f O aO `op` f I aI

diag :: Two a -> Two a -> Two a
diag (a0 :* _) (_ :* a1) = a0 :* a1

fst :: Two a -> a
fst (a :* _) = a

snd :: Two a -> a
snd (_ :* a) = a
