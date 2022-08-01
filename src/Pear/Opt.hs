{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Pear.Opt where

import Pear.Bit
import Pear.Bit.Singleton
import Pear.Bit.Finite
import Pear.Singleton.Applicative
import Pear.Singleton.Known
import Pear.Indexed
import Pear.Via.Deindexed
import Pear.Via.Elem

type Opt :: Bit -> * -> *
data Opt (length :: Bit) a where
  None :: Opt 'O a
  Some :: { getSome :: !a } -> Opt 'I a

deriving instance Show a => Show (Opt b a)
deriving instance Eq a => Eq (Opt b a)
deriving instance Ord a => Ord (Opt b a)

deriving instance Functor (Opt b)
deriving instance Foldable (Opt b)
deriving instance Traversable (Opt b)

deriving via Deindexed (Opt b) instance Known b => Applicative (Opt b)
deriving via Deindexed (Opt b) instance Known b => Monad (Opt b)

-- might need to rework these
deriving via (Opt b ∈ ITraversable) instance IFunctor (Opt b)
deriving via (Opt b ∈ ITraversable) instance IFoldable (Opt b)
deriving via (Opt ∈ SApplicative) b instance Known b => IApplicative (Opt b)

instance Indexed (Opt b) where
  type Ix (Opt b) = FBit b
  at Z f (Some a) = Some <$> f a

instance ITraversable (Opt b) where
  itraverse _ None = pure None
  itraverse f (Some a) = Some <$> f Z a

instance SApplicative Opt where
  spure SO _ = None
  spure SI f = Some (f Z)

  liftSA2 SO _ None None = None
  liftSA2 SI f (Some a) (Some b) = Some do f Z a b

instance Known b => IMonad (Opt b) where
  ibind _ None = None
  ibind f (Some a) = f Z a

{-# ANN toOpt "HLint: ignore Use const" #-}
-- `const` doesn't work in this case, due to the constraint on the ignored
-- parameter;
--
--    src/Pear/Opt.hs:72:9-18: error:
--        • Couldn't match type ‘'O’ with ‘'I’
--        • In the expression: const None
--          In a case alternative: SO -> const None
--          In the expression:
--            case sing @b of
--              SO -> const None
--              SI -> Some
--       |
--    72 |   SO -> const None
--       |         ^^^^^^^^^^
toOpt :: forall b a. Known b => (b ~ 'I => a) -> Opt b a
toOpt = case sing @b of
  SO -> \_ -> None
  SI -> Some

mapOpt :: (b ~ 'I => x -> y) -> Opt b x -> Opt b y
mapOpt _ None = None
mapOpt f (Some x) = Some (f x)

zipOpt :: (b ~ 'I => x -> y -> z) -> Opt b x -> Opt b y -> Opt b z
zipOpt _ None None = None
zipOpt f (Some x) (Some y) = Some (f x y)

bindOpt :: Opt b x -> (b ~ 'I => x -> Opt b y) -> Opt b y
bindOpt None _ = None
bindOpt (Some a) f = f a

foldOpt :: Monoid m => (b ~ 'I => a -> m) -> Opt b a -> m
foldOpt _ None = mempty
foldOpt f (Some a) = f a

traverseOpt :: Applicative f => (b ~ 'I => x -> f y) -> Opt b x -> f (Opt b y)
traverseOpt _ None = pure None
traverseOpt f (Some a) = Some <$> f a

seqOpt :: Opt b a -> (Known b => r) -> r
seqOpt None r = r
seqOpt (Some _) r = r
