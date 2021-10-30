{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE BlockArguments                #-}
{-# LANGUAGE MultiParamTypeClasses                #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE ViewPatterns       #-}
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
  , type (<->)(..)
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
  Some :: !a -> Opt ('SomeBit 'I) a

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
data a <-> b = Bijection {to :: a -> b, fro :: b -> a}

infix 0 <->

type Vec' m = Vec ('Binary m)
type Fin' m = Fin ('Binary m)
type Opt' b = Opt ('SomeBit b)

data FinCarry m n b where
  LHS :: forall p (m :: Bits 'NonZero p) n b. Fin' m  -> FinCarry ('Binary m) n b
  RHS :: forall p m (n :: Bits 'NonZero p) b. Fin' n -> FinCarry m ('Binary n) b
  CarryBit :: FinCarry m n ('SomeBit 'I)

type FinCarry' m n b = FinCarry ('Binary m) ('Binary n) ('SomeBit b)

-- O(min(log(m),log(n)))
combine :: (m ~ 'Binary m', n ~ 'Binary n') => Vec m a -> Vec n a -> (Vec (m + n) a, Either (Fin m) (Fin n) <-> Fin (m + n))
combine = \lhs rhs -> simplify <$> combineCarry lhs rhs None
  where
    simplify :: (FinCarry' m n 'O <-> p) -> (Either (Fin' m) (Fin' n) <-> p)
    simplify Bijection {to, fro} =
      Bijection
      { to = either (to . mkLHS) (to . mkRHS)
      , fro = fro >>> \case
          LHS ix -> Left ix
          RHS ix -> Right ix
      }

mkLHS :: Fin' m -> FinCarry ('Binary m) n b
mkLHS = \case
  ix@(_ :! _) -> LHS ix
  ix@Top -> LHS ix

mkRHS :: Fin' n -> FinCarry m ('Binary n) b
mkRHS = \case
  ix@(_ :! _) -> RHS ix
  ix@Top -> RHS ix

combineCarry 
  :: Vec' m a -> Vec' n a -> Opt' b a
  -> (Vec' (AddCarry m n b) a, FinCarry' m n b <-> Fin' (AddCarry m n b))
-- ≥2 zeroes, empty
combineCarry Empty Empty None = ( Empty, Bijection { to = \case, fro = \case } )
combineCarry Empty vec None = ( vec, Bijection { to = \case RHS ix -> ix, fro = mkRHS })
combineCarry vec Empty None = ( vec, Bijection { to = \case LHS ix -> ix, fro = mkLHS })
combineCarry Empty Empty (Some a) =
  ( Leaf a
  , Bijection
    { to =  \case CarryBit -> Top
    , fro = \case Top -> CarryBit
    }
  )
-- ≥2 zeroes, branch and branch
combineCarry (lhs :& None) (rhs :& opt) None =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs None
   in ( vec :& opt
      , Bijection
        { to = \case 
            LHS (ix :! b) -> to (LHS ix) :! b
            RHS (ix :! b) -> to (RHS ix) :! b
            RHS Top -> Top
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
            Top -> RHS Top
        }
      )
combineCarry (lhs :& opt) (rhs :& None) None =
  let (vec, Bijection {to, fro}) = combineCarry lhs rhs None
   in ( vec :& opt
      , Bijection
        { to = \case
            LHS (ix :! b) -> to (LHS ix) :! b
            LHS Top -> Top
            RHS (ix :! b) -> to (RHS ix) :! b
        , fro = \case
            ix :! b -> case fro ix of
              LHS ix -> LHS (ix :! b)
              RHS ix -> RHS (ix :! b)
            Top -> LHS Top
        }
      )
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

-- after

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
