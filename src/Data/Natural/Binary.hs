{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Data.Natural.Binary where

import Data.Natural.Binary.Two
import Data.Natural.Binary.Type

import Data.Functor.Const
import Data.Functor.Identity
import GHC.Types (Constraint)
import Control.Monad (ap)
import Prelude hiding ((!!), reverse)
import Control.Applicative (liftA2)

infixl 4 :&

type Opt :: Bit -> * -> *
data Opt (length :: Bit) a where
  None :: Opt 'O a
  Some :: { getSome :: !a } -> Opt 'I a

deriving instance Show a => Show (Opt m a)
deriving instance Eq a => Eq (Opt m a)
deriving instance Ord a => Ord (Opt m a)
deriving instance Functor (Opt m)
deriving instance Foldable (Opt m)
deriving instance Traversable (Opt m)

class KnownBit b where
  toOpt :: (b ~ 'I => a) -> Opt b a

instance KnownBit 'O where
  toOpt _ = None

instance KnownBit 'I where
  toOpt = Some

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

seqOpt :: Opt b a -> (KnownBit b => r) -> r
seqOpt None r = r
seqOpt (Some _) r = r

instance KnownBit b => Applicative (Opt b) where
  pure = toOpt
  (<*>) = ap

instance KnownBit b => Monad (Opt b) where
  None >>= _ = None
  Some a >>= f = f a

type Vec :: Binary -> * -> *
data Vec (length :: Binary) a where
  Nil :: Vec 'Ob a
  (:&) :: { getForest :: !(Vec bs (Two a)), getSubtree :: !(Opt b a) } -> Vec (bs ':. b) a

deriving instance Show a => Show (Vec m a)
deriving instance Eq a => Eq (Vec m a)
deriving instance Ord a => Ord (Vec m a)
deriving instance Functor (Vec m)
deriving instance Foldable (Vec m)
deriving instance Traversable (Vec m)

instance KnownBits m => Applicative (Vec m) where
  pure = toVec . const
  (<*>) = ap

instance KnownBits m => Monad (Vec m) where
  v >>= f = v `bindWithIndex` const f

mapWithIndex :: (Fin m -> a -> b) -> Vec m a -> Vec m b
mapWithIndex f = runIdentity . traverseWithIndex (\ix -> Identity . f ix)

foldWithIndex :: Monoid n => (Fin m -> a -> n) -> Vec m a -> n
foldWithIndex f = getConst . traverseWithIndex (\ix -> Const . f ix)

traverseWithIndex :: Applicative f => (Fin m -> x -> f y) -> Vec m x -> f (Vec m y)
traverseWithIndex _ Nil = pure Nil
traverseWithIndex f (veca :& opta) = liftA2 (:&) 
  do traverseWithIndex (\ix (aO :* aI) -> (:*) <$> f (ix :! O) aO <*> f (ix :! I) aI) veca
  do traverseOpt (f Top) opta

zipWithIndex :: (Fin m -> a -> b -> c) -> Vec m a -> Vec m b -> Vec m c
zipWithIndex _ Nil Nil = Nil
zipWithIndex f (veca :& opta) (vecb :& optb) = (:&)
  do zipWithIndex (\ix a b -> (f (ix :! O) :* f (ix :! I)) <*> a <*> b) veca vecb
  do zipOpt (f Top) opta optb

bindWithIndex :: Vec m a -> (Fin m -> a -> Vec m b) -> Vec m b
bindWithIndex vec f = mapWithIndex (\ix a -> f ix a !! ix) vec

(!!) :: Vec m a -> Fin m -> a
(!!) = go id where
  go :: (a -> b) -> Vec n a -> Fin n -> b
  go f (_ :& Some a) Top = f a
  go f (vec :& _) (ix :! b) = go (f . at b) vec ix
  
  at :: Bit -> Two a -> a
  at O (a :* _) = a
  at I (_ :* a) = a

class KnownBits m where
  toVec :: (Fin m -> a) -> Vec m a

instance KnownBits 'Ob where
  toVec _ = Nil
  
instance (KnownBits bs, KnownBit b) => KnownBits (bs ':. b) where
  toVec f = toVec (\ix -> f (ix :! O) :* f (ix :! I)) :& toOpt (f Top)

indexes :: KnownBits m => Vec m (Fin m)
indexes = toVec id

data Fin (cardinality :: Binary) where
  (:!) :: !(Fin bs) -> !Bit -> Fin (bs ':. b)
  Top :: Fin (bs ':. 'I)

infixl 4 :!

type Combined :: Binary -> Binary -> Bit -> * -> *
data Combined m n b a = Combined
  { vector :: Vec (Add m n b) a
  , to :: To m n b 
  , fro :: Fro m n b 
  }

type To m n b = FinOf m n b -> Fin (Add m n b)
type Fro m n b = Fin (Add m n b) -> FinOf m n b

type FinOf :: Binary -> Binary -> Bit -> *
data FinOf m n b where
  LHS :: !(Fin m) -> FinOf m n b
  RHS :: !(Fin n) -> FinOf m n b
  CarryBit :: FinOf m n 'I

type HeadBin :: Binary -> Constraint
class HeadBin m where
  snoc :: Fin (HighBits m) -> Bit -> Fin m
  top :: Dict (LowBit m ~ 'I) -> Fin m

instance HeadBin 'Ob where
  snoc = \case
  top = \case

instance HeadBin (bs ':. 'O) where
  snoc = (:!)
  top = \case

instance HeadBin (bs ':. 'I) where
  snoc = (:!)
  top Dict = Top

{-
type KnownBin :: Binary -> Constraint
class KnownBin m where
  index :: Vec m (Fin m)

instance KnownBin 'Ob where
  index = Nil

instance KnownBin bs => KnownBin (bs ':. b) where
  index = 
  -}

type Dict :: Constraint -> *
data Dict c where
  Dict :: c => Dict c

-- would be O(1), except for strictness
binaryVec :: Vec m a -> Dict (HeadBin m)
binaryVec Nil = Dict
binaryVec (_ :& Some _) = Dict
binaryVec (_ :& None) = Dict

combine :: Vec m a -> Vec n a -> Combined m n 'O a
combine v v' = combineCarry v v' None

combineCarry :: Vec m a -> Vec n a -> Opt b a -> Combined m n b a
combineCarry v@(binaryVec -> Dict) v'@(binaryVec -> Dict) = combineCarry' v v'

combineCarry' :: (HeadBin m, HeadBin n) => Vec m a -> Vec n a -> Opt b a -> Combined m n b a
combineCarry' Nil Nil None = Combined
  do Nil 
  do \case
  do \case
combineCarry' Nil Nil opt@(Some _) = Combined 
  do Nil :& opt
  do \case CarryBit -> Top
  do \case Top -> CarryBit
combineCarry' Nil (rvec :& ropt) bopt = combineCarryR Nil None rvec ropt bopt
combineCarry' (lvec :& lopt) Nil bopt = combineCarryR lvec lopt Nil None bopt
combineCarry' (lvec :& lopt) (rvec :& ropt) bopt = combineCarryR lvec lopt rvec ropt bopt

combineCarryR :: 
  ( Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b)':. Xor (LowBit m) (LowBit n) b )
  , HeadBin m
  , HeadBin n
  ) =>
  Vec (HighBits m) (Two a) -> Opt (LowBit m) a -> 
  Vec (HighBits n) (Two a) -> Opt (LowBit n) a -> 
  Opt b a -> Combined m n b a
combineCarryR lvec lopt rvec ropt bopt = withCombined
  do (:& xopt)
  do toFor lopt ropt bopt
  do froFor lopt ropt bopt
  do combineCarry lvec rvec copt
  where 
    (copt, xopt) = carry lopt ropt bopt

carry :: Opt x a -> Opt y a -> Opt z a -> (Opt (Carry x y z) (Two a), Opt (Xor x y z) a)
carry None None opt = (None, opt)
carry None opt@(Some _) None = (None, opt)
carry opt@(Some _) None None = (None, opt)
carry (Some la) (Some ra) opt = (Some (la :* ra), opt)
carry (Some la) None (Some ba) = (Some (la :* ba), None)
carry None (Some ra) (Some ba) = (Some (ra :* ba), None)

froFor :: 
  ( Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b)
  , HeadBin m
  , HeadBin n
  ) =>
  Opt (LowBit m) i -> 
  Opt (LowBit n) j -> 
  Opt b k -> 
  Fro (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) -> 
  Fro m n b
froFor None None _ fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> CarryBit
froFor None (Some _) None fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> RHS (top Dict)
froFor (Some _) None None fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> LHS (top Dict)
froFor (Some _) (Some _) _ fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> LHS (top Dict)
      I -> RHS (top Dict)
  Top -> CarryBit
froFor (Some _) None (Some _) fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> LHS (top Dict)
      I -> CarryBit
froFor None (Some _) (Some _) fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> RHS (top Dict)
      I -> CarryBit

toFor :: 
  (Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b)) =>
  Opt (LowBit m) i -> Opt (LowBit n) j -> Opt b k -> To (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) -> To m n b
toFor None None _ to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  CarryBit -> Top
toFor None (Some _) None to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> Top
toFor (Some _) None None to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> Top
  RHS (bs :! b) -> to (RHS bs) :! b
toFor (Some _) (Some _) _ to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> to CarryBit :! O
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> to CarryBit :! I
  CarryBit -> Top
toFor (Some _) None (Some _) to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> to CarryBit :! O
  RHS (bs :! b) -> to (RHS bs) :! b
  CarryBit -> to CarryBit :! I
toFor None (Some _) (Some _) to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> to CarryBit :! O
  CarryBit -> to CarryBit :! I

withCombined :: 
  (Vec (Add m' n' b') a' -> Vec (Add m n b) a) ->
  (To m' n' b' -> To m n b) ->
  (Fro m' n' b' -> Fro m n b) ->
  Combined m' n' b' a' -> Combined m n b a
withCombined withVector withTo withFro Combined{vector,to,fro} = Combined
  { vector = withVector vector
  , to = withTo to
  , fro = withFro fro
  }

type LowBit :: Binary -> Bit
type family LowBit m where
  LowBit 'Ob = 'O
  LowBit (bs ':. b) = b

type HighBits :: Binary -> Binary
type family HighBits m where
  HighBits 'Ob = 'Ob
  HighBits (bs ':. b) = bs

type Carry :: Bit -> Bit -> Bit -> Bit
type family Carry x y z where
  Carry 'O 'O b = 'O
  Carry 'O 'I b = b
  Carry 'I 'O b = b
  Carry 'I 'I b = 'I
  
type Xor :: Bit -> Bit -> Bit -> Bit
type family Xor x y z where
  Xor p q q = p
  Xor q p q = p
  Xor q q p = p

type Add :: Binary -> Binary -> Bit -> Binary
type family Add m n b where
  Add 'Ob 'Ob 'O = 'Ob
  Add 'Ob 'Ob 'I = 'Ob ':. 'I
  Add m n b = Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b


{-
type (+) :: Binary -> Binary -> Binary

type as + bs = AddBits 'O as bs

type AddBits :: Bit -> Binary -> Binary -> Binary
type family AddBits carry lhs rhs where
  AddBits c (as ':. a) (bs ':. b) = SumBit a b AddBits ':. c (CarryBit a b c) as bs
  AddBits 'O 'Ob rhs = rhs
  AddBits 'O lhs 'Ob = lhs
  AddBits 'I 'Ob rhs = AddBits 'O '[ 'I] rhs
  AddBits 'I lhs 'Ob = AddBits 'O lhs '[ 'I]

type SumBit :: Bit -> Bit -> Bit -> Bit
type family SumBit a b c where
  SumBit 'O 'O c = c
  SumBit 'I 'I c = c
  SumBit 'O 'I c = FlipBit c
  SumBit 'I 'O c = FlipBit c

type CarryBit :: Bit -> Bit -> Bit -> Bit
type family CarryBit a b c where
  CarryBit 'O 'O c = 'O
  CarryBit 'O 'I c = c
  CarryBit 'I 'O c = c
  CarryBit 'I 'I c = 'I

type FlipBit :: Bit -> Bit
type family FlipBit a = a' | a' -> a where
  FlipBit 'O = 'I
  FlipBit 'I = 'O

vacant :: Fin 'Ob -> a
vacant = \case

deriving instance Eq (Fin (cardinality))

deriving instance Ord (Fin (cardinality))

deriving instance Show (Fin cardinality)

deriving instance Functor (Opt length)

instance Applicative (Opt 'O) where
  pure = const None
  _ <*> _ = None

instance Applicative (Opt 'I) where
  pure = Some
  Some f <*> Some a = Some (f a)

instance Foldable (Opt b) where
  foldMap _ None = mempty
  foldMap f (Some a) = f a

instance Traversable (Opt b) where
  traverse _ None = pure None
  traverse f (Some a) = Some <$> f a

deriving instance Functor Two

instance Applicative Two where
  pure a = a :* a
  (f :* g) <*> (a :* b) = f a :* g b

instance Foldable Two where
  foldMap f (a :* b) = f a <> f b

instance Traversable Two where
  traverse f (a :* b) = (:*) <$> f a <*> f b

(*!) :: Two a -> Bit -> a
(a :* _) *! O = a
(_ :* a) *! I = a

deriving instance Functor (Vec length)

--- Applicative will require KnownBits

instance Foldable (Vec length) where
  foldMap _ Nil = mempty
  foldMap f (vec :& opt) = foldMap (foldMap f) vec <> foldMap f opt

instance Traversable (Vec length) where
  traverse _ Nil = pure Nil
  traverse f (vec :& opt) = (:&) <$> traverse (traverse f) vec <*> traverse f opt

(.!) :: Vec n a -> Fin n -> a
(.!) = go id
  where
    go :: (a -> b) -> Vec n a -> Fin n -> b
    go get (_ :& Some a) Z = get a
    go get (vec :& _) (ix :! b) = go (get . (*! b)) vec ix

data a <-> b = Bijection {to :: a -> b, fro :: b -> a}

push :: a -> Vec n a -> Vec ('[ 'I] + n) a
push a Nil = Nil :& Some a
push a (vec :& None) = vec :& Some a
push a (vec :& Some a') = push (a :* a') vec :& None

type Succ :: Binary -> Binary
type family Succ n = n' where
  Succ (bs ':. 'I) = Succ ':. 'O bs
  Succ (bs ':. 'O) = bs ':. 'I
  Succ 'Ob = '[ 'I]

type Pred :: Binary -> Binary
type family Pred n = n' | n' -> n where
  Pred (bs ':. 'O) = Pred ':. 'I bs
  Pred ('Ob ':. 'I) = 'Ob
  Pred (bs ':. 'I) = bs ':. 'O

-- pop :: Vec ('[ 'I] + n) a -> (a, Vec n a)
pop :: Vec n a -> (a, Vec (Pred n) a)
-- pop Nil = undefined
pop (Nil :& Some a) = (a, Nil)
pop (vec@(_ :& _) :& Some a) = (a, vec :& None)
pop (vec :& None) = (a', vec' :& Some a)
  where
    (a :* a', vec') = pop vec

{-
-}

{-
-- O(min(log(m),log(n)))
combine :: Vec m a -> Vec n a -> (Vec (m + n) a, Either (Fin m) (Fin n) <-> Fin (m + n))
combine = combine' None
  where
    combine' :: Opt b a -> Vec m a -> Vec n a -> (Vec (m + n + '[b]) a, Either (Fin m) (Fin n) <-> Fin (m + n))
    combine' = undefined
-}
{-
combine Nil v = (v, Bijection {to = either vacant id, fro = Right})
combine v Nil = (v, Bijection {to = either id vacant, fro = Left})
combine (v :& Some a) (v' :& Some a') = undefined v v'
combine (v :& None) (v' :& None) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :& None,
      Bijection
        { to = either
            \case (ix :! b) -> to (Left ix) :! b
            \case (ix :! b) -> to (Right ix) :! b,
          fro = \case (ix :! b) -> either (Left . (:! b)) (Right . (:! b)) (fro ix)
        }
    )
combine (v :& None) (v' :& opt@(Some _)) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :& opt,
      Bijection
        { to = either
            \case (ix :! b) -> to (Left ix) :! b
            \case
              (ix :! b) -> to (Right ix) :! b
              Z -> Z,
          fro = \case
            (ix :! b) -> either (Left . (:! b)) (Right . (:! b)) (fro ix)
            Z -> Right Z
        }
    )
combine (v :& opt@(Some _)) (v' :& None) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :& opt,
      Bijection
        { to = either
            \case
              (ix :! b) -> to (Left ix) :! b
              Z -> Z
            \case (ix :! b) -> to (Right ix) :! b,
          fro = \case
            (ix :! b) -> either (Left . (:! b)) (Right . (:! b)) (fro ix)
            Z -> Left Z
        }
    )
    -}
    -}
