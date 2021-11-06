{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing -Wno-unused-matches #-}

module Bin 
  ( Binary(..)
  , SomeBit(..)
  , Bits(..)
  , Bit(..)
  , Parity(..)
  , IsZero(..)
  , bitsCompare
  , bitCompare
  , Fin(..)
  , Vec(..)
  , Opt(..)
  , Two(..)
  , (!!)
  , combine
  -- , mapWithIndex :: (Fin m -> a -> b) -> Vec m a -> Vec m b
  -- , withList :: (forall bs. Vec ('Binary bs) a -> r) -> [a] -> r
  -- , instance Show
  , (+)
  , AddToZero
  , AddParity
  , AddCarry
  -- , zip -- just applicative
  -- , zipWith
  -- , (++) -- might be better just reindexing after combine
  -- , reverse
  -- , reverseOnto
  -- , pop
  -- , push
  ) where

import Control.Category ((>>>))
import Prelude hiding ((!!), (++), reverse)
import Control.Applicative (liftA2)

-- see Bin.hs for less strongly typed alternative


data Parity = Even | Odd deriving (Show, Eq, Ord)

data IsZero = Zero | NonZero deriving (Show, Eq, Ord)

data Bit (parity :: Parity) where
  O :: Bit 'Even
  I :: Bit 'Odd

deriving instance Eq (Bit parity)

deriving instance Ord (Bit parity)

deriving instance Show (Bit parity)

data SomeBit where
  SomeBit :: Bit parity -> SomeBit

data Bits (isZero :: IsZero) (parity :: Parity) where
  ObO :: Bits 'Zero 'Even
  ObI :: Bits 'NonZero 'Odd
  (:.) :: forall parity' parity. Bits 'NonZero parity' -> Bit parity -> Bits 'NonZero parity

infixl 4 :.

bitCompare :: Bit parity -> Bit parity' -> Ordering
bitCompare O O = EQ
bitCompare I I = EQ
bitCompare O I = LT
bitCompare I O = GT

bitsCompare :: Bits isZero parity -> Bits isZero' parity' -> Ordering
bitsCompare (as :. a) (bs :. b) = bitsCompare as bs <> bitCompare a b
bitsCompare (_ :. _) _          = GT
bitsCompare _ (_ :. _)          = LT
bitsCompare ObI ObI             = EQ
bitsCompare ObI ObO             = GT
bitsCompare ObO ObI             = LT
bitsCompare ObO ObO             = EQ

instance Ord (Bits isZero parity) where
  compare = bitsCompare

instance Eq (Bits isZero parity) where
  a == b = compare a b == EQ

deriving instance Show (Bits isZero parity)

data Binary where
  Binary :: Bits isZero parity -> Binary

data Fin (cardinality :: Binary) where
  (:!) :: Fin ('Binary bs) -> Bit p -> Fin ('Binary (bs ':. b))
  Top :: forall (bs :: Bits 'NonZero 'Odd). Fin ('Binary bs)

infixl 4 :!

instance Eq (Fin cardinality) where
  a == b = compare a b == EQ

instance Ord (Fin cardinality) where
  (prefix :! parity) `compare` (prefix' :! parity') = compare prefix prefix' <> bitCompare parity parity'
  (_ :! _) `compare` Top = LT
  Top `compare` (_ :! _) = GT
  Top `compare` Top = EQ

deriving instance Show (Fin cardinality)

data Opt (length :: SomeBit) a where
  None :: Opt ('SomeBit 'O) a
  Some :: { getSome :: !a } -> Opt ('SomeBit 'I) a

deriving instance Functor (Opt length)

instance Applicative (Opt ('SomeBit 'O)) where
  pure = const None
  _ <*> _ = None

instance Monad (Opt ('SomeBit 'O)) where
  _ >>= _ = None

instance Foldable (Opt ('SomeBit 'O)) where
  foldMap _ _ = mempty

instance Traversable (Opt ('SomeBit 'O)) where
  traverse _ _ = pure None

instance Applicative (Opt ('SomeBit 'I)) where
  pure = Some
  Some f <*> Some a = Some (f a)

instance Monad (Opt ('SomeBit 'I)) where
  Some a >>= f = f a

instance Foldable (Opt ('SomeBit 'I)) where
  foldMap f (Some a) = f a

instance Traversable (Opt ('SomeBit 'I)) where
  traverse f (Some a) = Some <$> f a

data Two a = !a :* !a

deriving instance Functor Two

instance Applicative Two where
  pure a = a :* a
  (f0 :* f1) <*> (a0 :* a1) = f0 a0 :* f1 a1

instance Monad Two where
  (a0 :* a1) >>= f = (b0 :* b1)
    where (b0 :* _) = f a0
          (_ :* b1) = f a1

instance Foldable Two where
  foldMap f (a0 :* a1) = f a0 <> f a1

instance Traversable Two where
  traverse f (a0 :* a1) = (:*) <$> f a0 <*> f a1

data Vec (length :: Binary) a where
  Empty :: Vec ('Binary 'ObO) a
  Leaf :: !a -> Vec ('Binary 'ObI) a
  (:&) :: !(Vec ('Binary bs) (Two a)) -> !(Opt ('SomeBit b) a) -> Vec ('Binary (bs ':. b)) a

infixl 4 :&

deriving instance Functor (Vec length)

instance Applicative (Vec ('Binary 'ObO)) where
  pure = const Empty
  _ <*> _ = Empty

instance Monad (Vec ('Binary 'ObO)) where
  _ >>= _ = Empty

instance Foldable (Vec ('Binary 'ObO)) where
  foldMap _ _ = mempty

instance Traversable (Vec ('Binary 'ObO)) where
  traverse _ _ = pure Empty

instance Applicative (Vec ('Binary 'ObI)) where
  pure = Leaf
  Leaf f <*> Leaf a = Leaf (f a)

instance Monad (Vec ('Binary 'ObI)) where
  Leaf a >>= f = f a
  
instance Foldable (Vec ('Binary 'ObI)) where
  foldMap f (Leaf a) = f a

instance Traversable (Vec ('Binary 'ObI)) where
  traverse f (Leaf a) = Leaf <$> f a

instance (Applicative (Vec ('Binary bs)), Applicative (Opt ('SomeBit b))) => Applicative (Vec ('Binary (bs ':. b))) where
  pure a = pure (pure a) :& pure a
  (fv :& fo) <*> (av :& ao) = (liftA2 (<*>) fv av) :& (fo <*> ao)

{-
-- diagonalize
-- may be easier to define in terms of join and index
instance Monad (Vec ('Binary (bs ':. b))) where
  (vec :& opt) >>= f = undefined

index :: Vec m a -> Vec m (Fin m, a)
-}

instance (Foldable (Vec ('Binary bs)), Foldable (Opt ('SomeBit b))) => Foldable (Vec ('Binary (bs ':. b))) where
  foldMap f (vec :& opt) = foldMap (foldMap f) vec <> foldMap f opt

instance (Traversable (Vec ('Binary bs)), Traversable (Opt ('SomeBit b))) => Traversable (Vec ('Binary (bs ':. b))) where
  traverse f (vec :& opt) = (:&) <$> traverse (traverse f) vec <*> traverse f opt

(!!) :: Vec n a -> Fin n -> a
(!!) = go id
  where
    go :: (a -> b) -> Vec n a -> Fin n -> b
    go jx (Leaf a) Top         = jx a
    go jx (_ :& (Some a)) Top  = jx a
    go jx (v :& _) (ix :! b) = go (jx . at b) v ix

    at :: Bit parity -> Two a -> a
    at O (a :* _) = a
    at I (_ :* a) = a

type Vec' m = Vec ('Binary m)
type Fin' m = Fin ('Binary m)
type Opt' b = Opt ('SomeBit b)

data FinCarry m n b where
  LHS :: forall p (m :: Bits 'NonZero p) n b. Fin' m  -> FinCarry ('Binary m) n b
  RHS :: forall p m (n :: Bits 'NonZero p) b. Fin' n -> FinCarry m ('Binary n) b
  CarryBit :: FinCarry m n ('SomeBit 'I)

type FinCarry' m n b = FinCarry ('Binary m) ('Binary n) ('SomeBit b)

mkLHS :: Fin' m -> FinCarry' m n b
mkLHS = \case
  ix@(_ :! _) -> LHS ix
  ix@Top -> LHS ix

mkRHS :: Fin' n -> FinCarry' m n b
mkRHS = \case
  ix@(_ :! _) -> RHS ix
  ix@Top -> RHS ix

-- O(min(log(m),log(n)))
combine :: (m ~ 'Binary m', n ~ 'Binary n') => Vec m a -> Vec n a -> Combined m n a
combine = \lhs rhs -> combineCarry lhs rhs None

type family Combined m n where
  Combined ('Binary m) ('Binary n) = CombinedCarry m n 'O

type (+) :: Binary -> Binary -> Binary
type family (+) lhs rhs where
  'Binary lhs + 'Binary rhs = 'Binary (AddCarry lhs rhs 'O)

type AddToZero :: Parity -> IsZero -> IsZero -> IsZero
type family AddToZero carry lhs rhs where
  AddToZero 'Even 'Zero p = p
  AddToZero 'Even p 'Zero = p
  AddToZero 'Odd _ _ = 'NonZero
  AddToZero _ 'NonZero _ = 'NonZero
  AddToZero _ _ 'NonZero = 'NonZero
  -- AddToZero _ _ _ = 'Zero

type AddParity :: Parity -> Parity -> Parity -> Parity
type family AddParity carry lhs rhs where
  AddParity 'Even 'Even p = p
  AddParity 'Even p 'Even = p
  AddParity p 'Even 'Even = p
  AddParity p 'Odd 'Odd = p
  AddParity 'Odd 'Odd p = p
  AddParity 'Odd p 'Odd = p

type AddCarry :: Bits isZero parity -> Bits isZero' parity' -> Bit carry -> Bits (AddToZero carry isZero isZero') (AddParity carry parity parity')
type family AddCarry lhs rhs carry = tot where
  -- ≥2 zeroes, empty
  AddCarry 'ObO xs 'O = xs
  AddCarry xs 'ObO 'O = xs
  AddCarry 'ObO 'ObO 'I = 'ObI
  -- ≥2 zeroes, branch and branch
  AddCarry (xs ':. 'O) (ys ':.  b) 'O = AddCarry xs ys 'O ':. b
  AddCarry (xs ':.  b) (ys ':. 'O) 'O = AddCarry xs ys 'O ':. b
  AddCarry (xs ':. 'O) (ys ':. 'O)  b = AddCarry xs ys 'O ':. b
  -- ≥2 zeroes, branch and leaf
  AddCarry (xs ':. 'O) 'ObI 'O = xs ':. 'I
  AddCarry 'ObI (xs ':. 'O) 'O = xs ':. 'I
  -- ≥2 zeroes, branch and empty
  AddCarry (xs ':. 'O) 'ObO  b = xs ':.  b
  AddCarry 'ObO (xs ':. 'O)  b = xs ':.  b
  -- ≥2 ones, branch and branch
  AddCarry (xs ':. 'I) (ys ':. 'I)  b = AddCarry xs ys 'I ':.  b
  AddCarry (xs ':. 'I) (ys ':. 'O) 'I = AddCarry xs ys 'I ':. 'O
  AddCarry (xs ':. 'O) (ys ':. 'I) 'I = AddCarry xs ys 'I ':. 'O
  -- ≥2 ones, branch and leaf
  AddCarry (xs ':. 'I) 'ObI  b = AddCarry xs 'ObO 'I ':.  b
  AddCarry 'ObI (xs ':. 'I)  b = AddCarry 'ObO xs 'I ':.  b
  AddCarry (xs ':. 'O) 'ObI 'I = AddCarry xs 'ObO 'I ':. 'O
  AddCarry 'ObI (xs ':. 'O) 'I = AddCarry 'ObO xs 'I ':. 'O
  -- ≥2 ones, branch and empty
  AddCarry (xs ':. 'I) 'ObO 'I = AddCarry xs 'ObO 'I ':. 'O
  AddCarry 'ObO (xs ':. 'I) 'I = AddCarry 'ObO xs 'I ':. 'O
  -- ≥2 ones, leaf and empty
  AddCarry 'ObI 'ObI  b = 'ObI ':.  b
  AddCarry 'ObI 'ObO 'I = 'ObI ':. 'O
  AddCarry 'ObO 'ObI 'I = 'ObI ':. 'O

type To m n b = FinCarry' m n b -> Fin' (AddCarry m n b)
type Fro m n b = Fin' (AddCarry m n b) -> FinCarry' m n b

data CombinedCarry m n b a = CombinedCarry
  { vector :: Vec' (AddCarry m n b) a
  , to :: To m n b
  , fro :: Fro m n b
  }

withCombinedCarry
  :: (Vec' (AddCarry m' n' b') a' -> Vec' (AddCarry m n b) a)
  -> (To m' n' b' -> To m n b)
  -> (Fro m' n' b' -> Fro m n b)
  -> CombinedCarry m' n' b' a' -> CombinedCarry m n b a
withCombinedCarry withVector withTo withFro CombinedCarry { vector, to, fro } = CombinedCarry
  { vector = withVector vector
  , to = withTo to
  , fro = withFro fro
  }

combineCarry :: Vec' m a -> Vec' n a -> Opt' b a -> CombinedCarry m n b a
combineCarry Empty Empty None = CombinedCarry 
  { vector = Empty
  , to = \case
  , fro = \case
  }
-- ≥2 zeroes, empty
combineCarry Empty vector None = CombinedCarry
  { vector
  , to = \case RHS ix -> ix
  , fro = mkRHS
  }
combineCarry vector Empty None = CombinedCarry
  { vector
  , to = \case LHS ix -> ix
  , fro = mkLHS
  }
combineCarry Empty Empty (Some a) = CombinedCarry
  { vector = Leaf a
  , to = \case CarryBit -> Top
  , fro = \case Top -> CarryBit
  }
-- ≥2 zeroes, branch and branch
combineCarry (lhs :& None) (rhs :& opt) None = withCombinedCarry
  do (:& opt)
  do to_mO_nb_O
  do fro_mO_nb_O
  do combineCarry lhs rhs None
combineCarry (lhs :& opt) (rhs :& None) None = withCombinedCarry
  do (:& opt)
  do to_mb_nO_O
  do fro_mb_nO_O
  do combineCarry lhs rhs None
combineCarry _ _ _ = undefined
{-
combineCarry (lhs :& None) (rhs :& None) opt@(Some _) =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs None
   in ( vec :& opt
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS (ix :! b) -> to (RHS ix) :! b
            CarryBit -> Top
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
            Top -> CarryBit
        }
      )
-- ≥2 zeroes, branch and leaf
combineCarry (vec :& None) (Leaf a) None =
    ( vec :& Some a
    , Bijection
      { to = \case
          LHS (ix :! b) -> ix :! b
          RHS Top -> Top
      , fro = \case
          ix :! b -> LHS (ix :! b)
          Top -> RHS Top
      }
    )
combineCarry (Leaf a) (vec :& None) None =
    ( vec :& Some a
    , Bijection
      { to = \case
          LHS Top -> Top
          RHS (ix :! b) -> ix :! b
      , fro = \case
          ix :! b -> RHS (ix :! b)
          Top -> LHS Top
      }
    )
-- ≥2 zeroes, branch and empty
combineCarry (vec :& None) Empty opt =
  ( vec :& opt
  , Bijection
    { to = \case
        LHS (ix :! b) -> ix :! b
        CarryBit -> Top
    , fro = \case
        ix :! b -> LHS (ix :! b)
        Top -> case opt of Some _ -> CarryBit
    }
  )
combineCarry Empty (vec :& None) opt =
  ( vec :& opt
  , Bijection
    { to = \case
        RHS (ix :! b) -> ix :! b
        CarryBit -> Top
    , fro = \case
        ix :! b -> RHS (ix :! b)
        Top -> case opt of Some _ -> CarryBit
    }
  )
-- ≥2 ones, branch and branch
combineCarry (lhs :& Some a) (rhs :& Some a') opt =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs (Some (a :* a'))
   in ( vec :& opt
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS (ix :! b) -> to (RHS ix) :! b
            LHS Top -> to CarryBit :! O
            RHS Top -> to CarryBit :! I
            CarryBit -> Top
        , fro = \case
            ix :! b -> case fro ix of 
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> RHS Top
            Top -> case opt of Some _ -> CarryBit
        }
      )
combineCarry (lhs :& Some a) (rhs :& None) (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS (ix :! b) -> to (RHS ix) :! b
            LHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> CarryBit
        }
      )
combineCarry (lhs :& None) (rhs :& Some a) (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS (ix :! b) -> to (RHS ix) :! b
            RHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> RHS Top
                I -> CarryBit
        }
      )
-- ≥2 ones, branch and leaf
combineCarry (lhs :& Some a) (Leaf a') opt =
  let (vec, Bijection {to, fro}) = combineCarry lhs Empty (Some (a :* a'))
   in ( vec :& opt
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            LHS Top -> to CarryBit :! O
            RHS Top -> to CarryBit :! I
            CarryBit -> Top
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> RHS Top
            Top -> case opt of Some _ -> CarryBit
        }
      )
combineCarry (Leaf a) (rhs :& Some a') opt =
  let (vec, Bijection {to, fro}) = combineCarry Empty rhs (Some (a :* a'))
   in ( vec :& opt
      , Bijection
        { to = \case
            RHS (ix :! b) -> to (RHS ix) :! b
            LHS Top -> to CarryBit :! O
            RHS Top -> to CarryBit :! I
            CarryBit -> Top
        , fro = \case
            ix :! b -> case fro ix of
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> RHS Top
            Top -> case opt of Some _ -> CarryBit
        }
      )
combineCarry (lhs :& None) (Leaf a) (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry lhs Empty (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              CarryBit -> case b of
                O -> RHS Top
                I -> CarryBit
        }
      )
combineCarry (Leaf a) (rhs :& None) (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry Empty rhs (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            RHS (ix :! b) -> to (RHS ix) :! b
            LHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> CarryBit
        }
      )
-- ≥2 ones, branch and empty
combineCarry (lhs :& Some a) Empty (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry lhs Empty (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            LHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              CarryBit -> case b of
                O -> LHS Top
                I -> CarryBit
        }
      )
combineCarry Empty (rhs :& Some a) (Some a') =
  let (vec, Bijection {to, fro}) = combineCarry Empty rhs (Some (a :* a'))
   in ( vec :& None
      , Bijection
        { to = \case
            RHS (ix :! b) -> to (RHS ix) :! b
            RHS Top -> to CarryBit :! O
            CarryBit -> to CarryBit :! I
        , fro = \case
            ix :! b -> case fro ix of
              RHS ix -> RHS (ix :! b)
              CarryBit -> case b of
                O -> RHS Top
                I -> CarryBit
        }
      )
-- ≥2 ones, leaf
combineCarry (Leaf a) (Leaf a') opt =
  ( Leaf (a :* a') :& opt
  , Bijection
    { to = \case
        LHS Top -> Top :! O
        RHS Top -> Top :! I
        CarryBit -> Top
    , fro = \case
        Top :! O -> LHS Top
        Top :! I -> RHS Top
        Top -> case opt of { Some _ -> CarryBit }
    }
  )
combineCarry (Leaf a) Empty (Some a') =
  ( Leaf (a :* a') :& None
  , Bijection
    { to = \case
        LHS Top -> Top :! O
        CarryBit -> Top :! I
    , fro = \case
      Top :! O -> LHS Top
      Top :! I -> CarryBit
    }
  )
combineCarry Empty (Leaf a) (Some a') =
  ( Leaf (a :* a') :& None
  , Bijection
    { to = \case
        RHS Top -> Top :! O
        CarryBit -> Top :! I
    , fro = \case
      Top :! O -> RHS Top
      Top :! I -> CarryBit
    }
  )

-}

to_mO_nb_O :: To m n 'O -> To (m ':. 'O) (n ':. b) 'O
to_mO_nb_O to_m_n_O = \case 
  LHS (ix :! b) -> to_m_n_O (LHS ix) :! b
  RHS (ix :! b) -> to_m_n_O (RHS ix) :! b
  RHS Top -> Top

fro_mO_nb_O :: Fro m n 'O -> Fro (m ':. 'O) (n ':. b) 'O
fro_mO_nb_O fro_m_n_O = \case
  ix :! b -> case fro_m_n_O ix of
    LHS ix -> LHS (ix :! b)
    RHS ix -> RHS (ix :! b)
  Top -> RHS Top

push :: forall b m n. (Fin' m -> Fin' n) -> Fin' (m ':. b) -> Fin' (n ':. b)
push f = \case
  (ix :! b) -> f ix :! b
  Top -> Top

push' :: forall b m n. (Fin' m -> Fin' n) -> Fin' (m ':. 'O) -> Fin' (n ':. b)
push' f = \case
  (ix :! b) -> f ix :! b

to_mb_nO_O :: To m n 'O -> To (m ':. b) (n ':. 'O) 'O
to_mb_nO_O = undefined

ex :: forall b (m :: Bits 'NonZero 'Odd).  Applicative (Opt' b) => Opt' b (Fin' m)
ex = pure Top

type Toward m n b t = FinCarry m n b -> Fin t
type Toward' m n b t = Toward ('Binary m) ('Binary n) ('SomeBit b) ('Binary t) 
type TowardInit' m n bc t = Toward' (Init m) (Init n) bc t

type Init :: Bits isZero parity -> Bits isZero' parity'
type family Init m where
  Init 'ObO = 'ObO
  Init 'ObI = 'ObO
  Init (bs ':. b) = bs

type Last :: Bits isZero parity -> Bit parity
type family Last m where
  Last 'ObO = 'O
  Last 'ObI = 'I
  Last (bs ':. b) = b

type Carry :: Parity -> Parity -> Parity -> Parity
type family Carry (bM :: Parity) (bN :: Parity) (b :: Parity) :: Parity where
  Carry 'Even 'Odd 'Odd = 'Odd
  Carry 'Odd 'Even 'Odd = 'Odd
  Carry 'Odd 'Odd 'Odd = 'Odd
  Carry 'Odd 'Odd 'Even = 'Odd
  Carry _ _ _ = 'Even

type Xor :: Parity -> Parity -> Parity -> Parity
type family Xor (bM :: Parity) (bN :: Parity) (b :: Parity) :: Parity where
  Xor p q q = p
  Xor q p q = p
  Xor q q p = p

converge
  :: forall 
     zm pm zn pn pb pt
     (m :: Bits zm pm)
     (n :: Bits zn pn)
     (b :: Bit pb)
     (t :: Bits 'NonZero pt)
     (bx :: Bit (Xor pm pn pb)) 
     (bc :: Bit (Carry pm pn pb))
  .  (TowardInit' m n bc t -> Opt' (Last m) (Fin' (t ':. bx)))
  -> (TowardInit' m n bc t -> Opt' (Last n) (Fin' (t ':. bx)))
  -> (TowardInit' m n bc t -> Opt' b (Fin' (t ':. bx)))
  -> TowardInit' m n b t 
  -> Toward' m n b (t ':. bx)
converge lset rset bset to = \case
  LHS (ix :! b) -> to (LHS ix) :! b
  LHS Top -> getSome (lset to)
  RHS (ix :! b) -> to (RHS ix) :! b
  RHS Top -> getSome (rset to)
  CarryBit -> getSome (bset to)
{-
to_mb_nO_O' :: forall m n b. To m n 'O -> FinCarry' (m ':. b) (n ':. 'O) 'O -> Fin' (AddCarry m n 'O ':. b)
-- to_mb_nO_O' = converge (const Top) (\case) (\case)
to_mb_nO_O' = converge @m @n undefined undefined undefined

converge
  :: forall 
    m n
    parityM parityN parityB
    (bM :: Bit parityM)
    (bN :: Bit parityN)
    (b :: Bit parityB)
    (bC :: Bit (Carry parityM parityN parityB))
    (bX :: Bit (Xor parityM parityN parityB))
  .  ((bM ~ 'I) => To m n bC -> Opt (Last' (m ':. bM)) (Fin' (AddCarry m n bC)))
  -> ((bN ~ 'I) => To m n bC -> Opt (Last' (n ':. bN)) (Fin' (AddCarry m n bC)))
  -> ((b ~ 'I) => To m n bC -> Opt' b (Fin' (AddCarry m n bC)))
  -> To m n bC -> FinCarry' (m ':. bM) (n ':. bN) b -> Fin' (AddCarry m n bC ':. bX)
converge lset rset bset to = \case
  LHS (ix :! b) -> to (LHS ix) :! b
  LHS Top -> getSome (lset to)
  RHS (ix :! b) -> to (RHS ix) :! b
  RHS Top -> getSome (rset to)
  CarryBit -> getSome (bset to)
  --mtry with m and n not ObO or ObI and then ramp up
-}
  {-
  :: forall 
            isZeroM parityM
            isZeroN parityN
            parityB
            (m :: Bits isZeroM parityM)
            (n :: Bits isZeroN parityN)
            (b :: Bit parityB)
            m' n' b' bT t
  .  ( 'Binary m' ~ Init' m
     , 'Binary n' ~ Init' n
     , 'SomeBit b' ~ Carry parityM parityN parityB
     , 'SomeBit bT ~ Xor parityM parityN parityB
     , t ~ (AddCarry m' n' b' ':. bT)
     ) 
  => (To m' n' b' -> Opt (Last' m) (Fin' t))
  -> (To m' n' b' -> Opt (Last' n) (Fin' t))
  -> (To m' n' b' -> Opt' b (Fin' t))
  -> To m' n' b' -> FinCarry' m n b -> Fin' t
  -}
{-
to_mb_nO_O = converge (push . (. LHS)) (push' . (. RHS)) (const None)

converge :: (x -> Fin' m -> Fin' t) -> (x -> Fin' n -> Fin' t) -> (x -> Opt' b (Fin' t)) -> x -> FinCarry' m n b -> Fin' t
converge f g t x = \case
  LHS ix -> f x ix
  RHS ix -> g x ix
  CarryBit -> getSome (t x)
-}

fro_mb_nO_O :: Fro m n 'O -> Fro (m ':. b) (n ':. 'O) 'O
fro_mb_nO_O fro_m_n_O = \case
  ix :! b -> case fro_m_n_O ix of
    LHS ix -> LHS (ix :! b)
    RHS ix -> RHS (ix :! b)
  Top -> LHS Top

-- we always do
--   to = \case
--      RHS (ix :! b) -> to' (RHS ix) :! b
--      LHS (ix :! b) -> to' (LHS ix) :! b
--
--   and
--
--   fro =case
--     ix :! b -> case fro' ix of
--       RHS ix -> RHS (ix :! b)
--       LHS ix -> LHS (ix :! b)
--
-- The only cases we need to paramtererize are 
--  to (RHS Top), to (LHS Top), to CarryBit
--  fro CarryBit, fro Top

{-
diverge :: Fro m n b -> _ -> _ -> Fro (m ':. b) (n ':. 'O) 'O
diverge f g t = \case
  ix :! b -> case f ix of
    LHS ix -> LHS (ix :! b)
    RHS ix -> RHS (ix :! b)
    CarryBit -> g b
  Top -> getSome t
  -}
