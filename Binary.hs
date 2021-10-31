{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Binary where

import GHC.Types (Constraint)

-- see Bin.hs for more strongly typed alternative

-- import Data.Monoid (Endo (..))
-- import qualified GHC.Natural as Base

data Bit = O | I deriving (Eq, Ord, Show)

-- '[I,O,I,I] => 0b1101
-- leading zeroes are legal, but non-canonical
type Bits = [Bit]

type Pair :: * -> *
data Pair a = !a :* !a

infixr 8 :*

type Vec :: Bits -> * -> *
data Vec (length :: Bits) a where
  Nil :: Vec '[] a
  (:&) :: !(Vec bs (Pair a)) -> !(Opt b a) -> Vec (b ': bs) a

infixl 4 :&

data Fin (cardinality :: Bits) where
  (:!) :: !(Fin bs) -> !Bit -> Fin (b ': bs)
  Top :: Fin ('I ': bs)

infixl 4 :!

type Opt :: Bit -> * -> *
data Opt (length :: Bit) a where
  None :: Opt 'O a
  Some :: !a -> Opt 'I a

type Combined :: Bits -> Bits -> Bit -> * -> *
data Combined m n b a = Combined
  { vector :: Vec (Add m n b) a
  , to :: To m n b 
  , fro :: Fro m n b 
  }

type To m n b = FinOf m n b -> Fin (Add m n b)
type Fro m n b = Fin (Add m n b) -> FinOf m n b

type FinOf :: Bits -> Bits -> Bit -> *
data FinOf m n b where
  LHS :: !(Fin m) -> FinOf m n b
  RHS :: !(Fin n) -> FinOf m n b
  CarryBit :: FinOf m n 'I

type Binary :: Bits -> Constraint
class Binary m where
  snoc :: Fin (HighBits m) -> Bit -> Fin m
  top :: Dict (LowBit m ~ 'I) -> Fin m

instance Binary '[] where
  snoc = \case
  top = \case

instance Binary ('O ': bs) where
  snoc = (:!)
  top = \case

instance Binary ('I ': bs) where
  snoc = (:!)
  top Dict = Top

type Dict :: Constraint -> *
data Dict c where
  Dict :: c => Dict c

binaryVec :: Vec m a -> Dict (Binary m)
binaryVec Nil = Dict
binaryVec (_ :& Some _) = Dict
binaryVec (_ :& None) = Dict

combine :: Vec m a -> Vec n a -> Combined m n 'O a
combine v v' = combineCarry v v' None

combineCarry :: Vec m a -> Vec n a -> Opt b a -> Combined m n b a
combineCarry v@(binaryVec -> Dict) v'@(binaryVec -> Dict) o = combineCarry' v v' o

combineCarry' :: (Binary m, Binary n) => Vec m a -> Vec n a -> Opt b a -> Combined m n b a
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
  ( Add m n b ~ (Xor (LowBit m) (LowBit n) b ': Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b))
  , Binary m
  , Binary n
  ) =>
  Vec (HighBits m) (Pair a) -> Opt (LowBit m) a -> 
  Vec (HighBits n) (Pair a) -> Opt (LowBit n) a -> 
  Opt b a -> Combined m n b a
combineCarryR lvec lopt rvec ropt bopt = withCombined
  do (:& xopt)
  do toFor lopt ropt bopt
  do froFor lopt ropt bopt
  do combineCarry lvec rvec copt
  where 
    (copt, xopt) = carry lopt ropt bopt

carry :: Opt x a -> Opt y a -> Opt z a -> (Opt (Carry x y z) (Pair a), Opt (Xor x y z) a)
carry None None opt = (None, opt)
carry None opt@(Some _) None = (None, opt)
carry opt@(Some _) None None = (None, opt)
carry (Some la) (Some ra) opt = (Some (la :* ra), opt)
carry (Some la) None (Some ba) = (Some (la :* ba), None)
carry None (Some ra) (Some ba) = (Some (ra :* ba), None)

froFor :: 
  ( Add m n b ~ (Xor (LowBit m) (LowBit n) b ': Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b))
  , Binary m
  , Binary n
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
  (Add m n b ~ (Xor (LowBit m) (LowBit n) b ': Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b))) =>
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

type LowBit :: Bits -> Bit
type family LowBit m where
  LowBit '[] = 'O
  LowBit (b ': bs) = b

type HighBits :: Bits -> Bits
type family HighBits m where
  HighBits '[] = '[]
  HighBits (b ': bs) = bs

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

type Add :: Bits -> Bits -> Bit -> Bits
type family Add m n b where
  Add '[] '[] 'O = '[]
  Add '[] '[] 'I = 'I ': '[]
  Add m n b = Xor (LowBit m) (LowBit n) b ': Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b)


{-
type (+) :: Bits -> Bits -> Bits

type as + bs = AddBits 'O as bs

type AddBits :: Bit -> Bits -> Bits -> Bits
type family AddBits carry lhs rhs where
  AddBits c (a ': as) (b ': bs) = SumBit a b c ': AddBits (CarryBit a b c) as bs
  AddBits 'O '[] rhs = rhs
  AddBits 'O lhs '[] = lhs
  AddBits 'I '[] rhs = AddBits 'O '[ 'I] rhs
  AddBits 'I lhs '[] = AddBits 'O lhs '[ 'I]

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

vacant :: Fin '[] -> a
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

deriving instance Functor Pair

instance Applicative Pair where
  pure a = a :* a
  (f :* g) <*> (a :* b) = f a :* g b

instance Foldable Pair where
  foldMap f (a :* b) = f a <> f b

instance Traversable Pair where
  traverse f (a :* b) = (:*) <$> f a <*> f b

(*!) :: Pair a -> Bit -> a
(a :* _) *! O = a
(_ :* a) *! I = a

deriving instance Functor (Vec length)

--- Applicative will require KnownBits

instance Foldable (Vec length) where
  foldMap _ Nil = mempty
  foldMap f (vec :. opt) = foldMap (foldMap f) vec <> foldMap f opt

instance Traversable (Vec length) where
  traverse _ Nil = pure Nil
  traverse f (vec :. opt) = (:.) <$> traverse (traverse f) vec <*> traverse f opt

(.!) :: Vec n a -> Fin n -> a
(.!) = go id
  where
    go :: (a -> b) -> Vec n a -> Fin n -> b
    go get (_ :. Some a) Z = get a
    go get (vec :. _) (ix :! b) = go (get . (*! b)) vec ix

data a <-> b = Bijection {to :: a -> b, fro :: b -> a}

push :: a -> Vec n a -> Vec ('[ 'I] + n) a
push a Nil = Nil :. Some a
push a (vec :. None) = vec :. Some a
push a (vec :. Some a') = push (a :* a') vec :. None

type Succ :: Bits -> Bits
type family Succ n = n' where
  Succ ('I ': bs) = 'O ': Succ bs
  Succ ('O ': bs) = 'I ': bs
  Succ '[] = '[ 'I]

type Pred :: Bits -> Bits
type family Pred n = n' | n' -> n where
  Pred ('O ': bs) = 'I ': Pred bs
  Pred ('I ': '[]) = '[]
  Pred ('I ': bs) = 'O ': bs

-- pop :: Vec ('[ 'I] + n) a -> (a, Vec n a)
pop :: Vec n a -> (a, Vec (Pred n) a)
-- pop Nil = undefined
pop (Nil :. Some a) = (a, Nil)
pop (vec@(_ :. _) :. Some a) = (a, vec :. None)
pop (vec :. None) = (a', vec' :. Some a)
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
combine (v :. Some a) (v' :. Some a') = undefined v v'
combine (v :. None) (v' :. None) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :. None,
      Bijection
        { to = either
            \case (ix :! b) -> to (Left ix) :! b
            \case (ix :! b) -> to (Right ix) :! b,
          fro = \case (ix :! b) -> either (Left . (:! b)) (Right . (:! b)) (fro ix)
        }
    )
combine (v :. None) (v' :. opt@(Some _)) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :. opt,
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
combine (v :. opt@(Some _)) (v' :. None) = case combine v v' of
  (vec, Bijection {to, fro}) ->
    ( vec :. opt,
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
