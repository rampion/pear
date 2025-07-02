{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Pear where
-- TODO: Article, then document
-- tests would be nice too

import Control.Category (Category(..)) -- we don't actually use this, but it pains me to not implement Category when the type so clearly supports it
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import Prelude hiding ((.), id)

type Bit :: Type
data Bit = O | I
  deriving (Show, Eq, Enum, Ord, Bounded)

type SBit :: Bit -> Type
data SBit b where
  SO :: SBit O
  SI :: SBit I

deriving instance Show (SBit b)
deriving instance Eq (SBit b)

type KnownBit :: Bit -> Constraint
class KnownBit b where
  knownBit :: SBit b

instance KnownBit O where knownBit = SO
instance KnownBit I where knownBit = SI

-- we could also make an FBit type, but it's just as convenient to do without

type Positive :: Type
data Positive = ObI | Positive :. Bit
  deriving (Eq, Ord)

infixl 4 :.

instance Show Positive where
  showsPrec p = showParen (p >= 4) . fix \loop -> \case
    n :. b -> loop n . showString " :. " . shows b
    ObI -> showString "ObI"

instance Enum Positive where
  succ = \case
    n :. I -> succ n :. O
    n :. O -> n :. I
    ObI -> ObI :. O

  toEnum = 
    fromMaybe (error "Positive.toEnum :: Invalid input") .
    safeToPositive

  fromEnum = \case
    ObI -> 1
    n :. b -> 2 * fromEnum n + fromEnum b

instance Bounded Positive where
  minBound = ObI
  maxBound = fix (:. I)

safeToPositive :: Int -> Maybe Positive
safeToPositive = \case
  n | n <= 0 -> Nothing
    | otherwise -> Just $ loop n
  where
    loop = \case
      1 -> ObI
      n -> 
        let (q, r) = n `quotRem` 2
        in loop q :. toEnum r

fix1 :: forall f a. ((forall i. f i -> a) -> (forall i. f i -> a)) -> (forall i. f i -> a)
fix1 f = g where
  g :: forall i. f i -> a
  g = f g

type SPositive :: Positive -> Type
data SPositive n where
  SObI :: SPositive ObI
  (:!) ::  SPositive n -> SBit b -> SPositive (n :. b)

infixl 4 :!

deriving instance Eq (SPositive n)

instance Show (SPositive n) where
  showsPrec p = showParen (p >= 4) . fix1 \loop -> \case
    sn :! sb -> loop sn . showString " :! " . shows sb
    SObI -> showString "SObI"

fromSPositive :: SPositive n -> Int
fromSPositive = loop 1 0 where
  loop :: Int -> Int -> SPositive n -> Int
  loop !p !t = \case
    SObI -> p + t
    sn :! sb -> loop (2*p) (t + case sb of SO -> 0 ; SI -> 1) sn

withSPositive :: (forall n. SPositive n -> r) -> Positive -> r
withSPositive k = \case
  ObI -> k SObI
  bs :. O -> withSPositive (k . (:! SO)) bs
  bs :. I -> withSPositive (k . (:! SI)) bs

type KnownPositive :: Positive -> Constraint
class KnownPositive n where
  knownPositive :: SPositive n

instance KnownPositive ObI where
  knownPositive = SObI

instance (KnownPositive n, KnownBit b) => KnownPositive (n :. b) where
  knownPositive = knownPositive @n :! knownBit @b

withKnownPositive :: ((KnownPositive n) => r) -> SPositive n -> r
withKnownPositive r = \case
  SObI -> r
  sn :! SO -> withKnownPositive r sn
  sn :! SI -> withKnownPositive r sn

type FPositive :: Positive -> Type
data FPositive n where
  (:?) :: FPositive n -> Bit -> FPositive (n :. b)
  FCanopy :: FPositive ObI
  FBranch :: FPositive (n :. I)

deriving instance Eq (FPositive n)
deriving instance Ord (FPositive n)

instance Show (FPositive n) where
  showsPrec p = showParen (p >= 4) . fix1 \loop -> \case
    fn :? b -> loop fn . showString " :? " . shows b
    FCanopy -> showString "FCanopy"
    FBranch -> showString "FBranch"

minBound' :: SPositive n -> FPositive n
minBound' = \case
  SObI -> FCanopy
  sn :! _ -> minBound' sn :? O

maxBound' :: SPositive n -> FPositive n
maxBound' = \case
  SObI -> FCanopy
  sn :! SO -> maxBound' sn :? I
  _ :! SI -> FBranch

instance KnownPositive n => Bounded (FPositive n) where
  minBound = minBound' knownPositive
  maxBound = maxBound' knownPositive

safeSucc :: KnownPositive n => FPositive n -> Maybe (FPositive n)
safeSucc = safeSucc' knownPositive

safeSucc' :: SPositive n -> FPositive n -> Maybe (FPositive n)
safeSucc' = loop Nothing Just where
  loop :: r -> (FPositive n -> r) -> SPositive n -> FPositive n -> r
  loop failure success = flip \case
    FCanopy -> \SObI -> failure
    FBranch -> \(_ :! SI) -> failure
    fn :? O -> \_ -> success (fn :? I)
    fn :? I -> \(sn :! sb) ->
      let failure' = case sb of
            SO -> failure
            SI -> success FBranch
      in
      loop failure' (success . (:? O)) sn fn

safePred :: KnownPositive n => FPositive n -> Maybe (FPositive n)
safePred = safePred' knownPositive

safePred' :: SPositive n -> FPositive n -> Maybe (FPositive n)
safePred' = loop id where
  loop :: (FPositive n -> r) -> SPositive n -> FPositive n -> Maybe r
  loop k = flip \cases
    FCanopy -> \SObI -> Nothing
    FBranch -> \(sn :! SI) -> Just $ k $ maxBound' sn :? I 
    (fn :? I) -> \_ -> Just $ k (fn :? O) 
    (fn :? O) -> \(sn :! _) -> loop (k . (:? I)) sn fn

safeToFPositive :: KnownPositive n => Int -> Maybe (FPositive n)
safeToFPositive 0 = Just minBound
safeToFPositive i = positiveToFPositive =<< safeToPositive i
  
positiveToFPositive :: KnownPositive n => Positive -> Maybe (FPositive n)
positiveToFPositive = positiveToFPositive' knownPositive

positiveToFPositive' :: SPositive n -> Positive -> Maybe (FPositive n)
positiveToFPositive' = loop Nothing id where
  loop :: Maybe r -> (FPositive n -> r) -> SPositive n -> Positive -> Maybe r
  loop eq lt = \cases
    SObI ObI -> eq
    SObI _   -> Nothing
    (sn :! _) ObI -> Just $ lt $ minBound' sn :? I
    (sn :! SO) (m :. O) -> loop eq (lt . (:? O)) sn m
    (sn :! SO) (m :. I) -> loop Nothing (lt . (:? I)) sn m
    (sn :! SI) (m :. O) -> loop (Just $ lt FBranch) (lt . (:? O)) sn m
    (sn :! SI) (m :. I) -> loop eq (lt . (:? I)) sn m

fromFPositive' :: SPositive n -> FPositive n -> Int
fromFPositive' = loop 1 0 where
  loop :: Int -> Int -> SPositive n -> FPositive n -> Int
  loop !p !t = \cases
    SObI FCanopy -> t
    (sn :! _) FBranch -> fromSPositive sn * 2 * p + t
    (sn :! _) (fn :? b) -> loop (2*p) (t + p * fromEnum b) sn fn

instance KnownPositive n => Enum (FPositive n) where
  succ =
    fromMaybe (error "FPositive.succ :: Invalid input") .
    safeSucc

  pred =
    fromMaybe (error "FPositive.pred :: Invalid input") .
    safePred
  
  toEnum = 
    fromMaybe (error "FPositive.toEnum :: Invalid input") .
    safeToFPositive

  fromEnum = fromFPositive' knownPositive

type Opt :: Bit -> Type -> Type
data Opt b a where
  None :: Opt O a
  Some :: a -> Opt I a

deriving instance Functor (Opt b)
deriving instance Foldable (Opt b)
deriving instance Traversable (Opt b)
deriving instance Eq a => Eq (Opt b a)
deriving instance Show a => Show (Opt b a)

instance KnownBit b => Applicative (Opt b) where
  liftA2 = liftO2

  pure a = case knownBit @b of
    SO -> None
    SI -> Some a

liftO2 :: (u -> v -> w) -> Opt b u -> Opt b v -> Opt b w
liftO2 f = \cases
  None None -> None 
  (Some u) (Some v) -> Some (f u v)

optSize :: Opt b a -> SBit b
optSize = \case
  None -> SO
  Some _ -> SI

type Pair :: Type -> Type
data Pair a = a :* a
  deriving (Functor, Foldable, Traversable, Eq, Show)

infix 8 :*

instance Applicative Pair where
  (f0 :* f1) <*> (a0 :* a1) = f0 a0 :* f1 a1
  pure a = a :* a

fix2 :: forall c f a . ((forall i x. c x => f i x -> a) -> (forall i x. c x => f i x -> a)) -> (forall i x. c x => f i x -> a)
fix2 f = g where
  g :: forall i x. c x => f i x -> a
  g = f g

type Tree :: Positive -> Type -> Type
data Tree n a where
  Canopy :: a -> Tree ObI a
  (:\) :: Tree n (Pair a) -> Opt b a -> Tree (n :. b) a

infixl 4 :\

deriving instance Functor (Tree n)
deriving instance Foldable (Tree n)
deriving instance Traversable (Tree n)
deriving instance Eq a => Eq (Tree n a)

instance Show a => Show (Tree n a) where
  showsPrec p = showParen (p >= 4) . fix2 @Show \loop -> \case
    taa :\ oa -> loop taa . showString " :\\ " . shows oa
    Canopy a -> showString "Canopy " . shows a

instance KnownPositive n => Applicative (Tree n) where
  liftA2 = liftT2
  pure a = generate (const a)

liftT2 :: (a -> b -> c) -> Tree m a -> Tree m b -> Tree m c
liftT2 f = \cases
  (taa :\ oa) (tbb :\ ob) -> liftT2 (liftA2 f) taa tbb :\ liftO2 f oa ob
  (Canopy a) (Canopy b) -> Canopy (f a b)

generate :: KnownPositive n => (FPositive n -> a) -> Tree n a
generate = flip generate' knownPositive

generate' :: (FPositive n -> a) -> SPositive n -> Tree n a
generate' f = \case
  SObI -> Canopy (f FCanopy)
  sn :! sb -> generate' (\fn -> f (fn :? O) :* f (fn :? I)) sn :\ case sb of
    SO -> None
    SI -> Some (f FBranch)

treeSize :: Tree n a -> SPositive n
treeSize = \case
  Canopy _ -> SObI
  taa :\ oa -> treeSize taa :! optSize oa

fromNonEmpty :: forall r a. (forall n. Tree n a -> r) -> NonEmpty a -> r
fromNonEmpty f = \(a :| as) -> loop (Canopy a) as where
  loop :: Tree n a -> [a] -> r
  loop t = \case
    [] -> f t
    a : as -> loop (push t a) as

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

_0 :: Lens' (Pair a) a
_0 f (a0 :* a1) = f a0 <&> (:* a1)

_1 :: Lens' (Pair a) a
_1 f (a0 :* a1) = (a0 :*) <$> f a1

atPair :: Bit -> Lens' (Pair a) a
atPair = \case
  O -> _0
  I -> _1

_Some :: Lens' (Opt I a) a
_Some f (Some a) = Some <$> f a

_Canopy :: Lens' (Tree ObI a) a
_Canopy f (Canopy a) = Canopy <$> f a

_Up :: Lens' (Tree (n :. b) a) (Tree n (Pair a))
_Up f (taa :\ oa) = f taa <&> (:\ oa)

_Off :: Lens' (Tree (n :. b) a) (Opt b a)
_Off f (taa :\ oa) = (taa :\) <$> f oa

_Branch :: Lens' (Tree (n :. I) a) a
_Branch = _Off . _Some

atTree :: FPositive n -> Lens' (Tree n a) a
atTree = \case
  FCanopy -> _Canopy
  FBranch -> _Branch
  fn :? b -> _Up . atTree fn . atPair b

type Iso :: Type -> Type -> Type
data Iso a b = Iso { forwards :: a -> b, backwards :: b -> a }

instance Category Iso where
  id = Iso id id
  Iso f0 b0 . Iso f1 b1 = Iso (f0 . f1) (b1 . b0)

type Succ :: Positive -> Positive
type Succ n = Add n ObI

type FSucc :: Positive -> Type
type FSucc n = FAdd n ObI

reindexSucc :: SPositive n -> Iso (FSucc n) (FPositive (Succ n))
reindexSucc sn = reindexAdd sn SObI

push :: Tree n a -> a -> Tree (Succ n) a
push t = fuse t . Canopy

type PoppedTree :: Positive -> Type -> Type
data PoppedTree n a where
  PoppedHas :: Tree n a -> PoppedTree (Succ n) a
  PoppedOut :: PoppedTree ObI a

pop :: Tree n a -> (PoppedTree n a, a)
pop = \case
  Canopy a0 -> (PoppedOut, a0)
  taa :\ Some a0 -> (PoppedHas (taa :\ None),  a0)
  taa :\ None -> case pop taa of
    (PoppedOut, a0 :* a1) -> (PoppedHas (Canopy a0),  a1)
    (PoppedHas taa, a0 :* a1) -> (PoppedHas (taa :\ Some a0), a1)

type Add :: Positive -> Positive -> Positive
type Add i j = AddC i j O

type FAdd :: Positive -> Positive -> Type
type FAdd n m = FAddC n m O

reindexAdd :: SPositive n -> SPositive m -> Iso (FAdd n m) (FPositive (Add n m))
reindexAdd sn sm = reindexAddC sn sm SO

fuse :: Tree n a -> Tree m a -> Tree (Add n m) a
fuse t0 t1 = fuseC t0 t1 None

fizz :: SPositive n -> SPositive m -> Tree (Add n m) a -> (Tree n a, Tree m a)
fizz sn sm t = (tn, tm)
  where (tn, tm, None) = fizzC sn sm SO t

type AddC :: Positive -> Positive -> Bit -> Positive
type family AddC i j b where
  AddC ObI ObI b = ObI :. b
  AddC (i :. O) ObI O = i :. I
  AddC (i :. O) ObI I = Succ i :. O
  AddC (i :. I) ObI b = Succ i :. b
  AddC ObI (j :. O) O = j :. I
  AddC ObI (j :. O) I = Succ j :. O
  AddC ObI (j :. I) b = Succ j :. b
  AddC (i :. O) (j :. O) b = Add i j :. b
  AddC (i :. O) (j :. I) O = Add i j :. I
  AddC (i :. O) (j :. I) I = AddC i j I :. O
  AddC (i :. I) (j :. O) O = Add i j :. I
  AddC (i :. I) (j :. O) I = AddC i j I :. O
  AddC (i :. I) (j :. I) b = AddC i j I :. b

type FAddC :: Positive -> Positive -> Bit -> Type
data FAddC n m b where
  FL :: FPositive n -> FAddC n m b
  FR :: FPositive m -> FAddC n m b
  FC :: FAddC n m I -- if we had an FBit type, here's the only place we'd actually use it

reindexAddC :: SPositive n -> SPositive m -> SBit b -> Iso (FAddC n m b) (FPositive (AddC n m b))
reindexAddC = \cases
  SObI SObI _ -> Iso
    { forwards = \case
        FL FCanopy -> FCanopy :? O
        FR FCanopy -> FCanopy :? I
        FC -> FBranch
    , backwards = \case
        FCanopy :? O -> FL FCanopy
        FCanopy :? I -> FR FCanopy
        FBranch -> FC
    }
  (_ :! SO) SObI SO -> Iso
    { forwards = \case
        FL (fn :? b) -> fn :? b
        FR FCanopy -> FBranch
    , backwards = \case
        fn :? b -> FL (fn :? b)
        FBranch -> FR FCanopy
    }
  (sn :! SO) SObI SI -> let iso = reindexSucc sn in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FR FCanopy -> forwards iso (FR FCanopy) :? O
        FC -> forwards iso (FR FCanopy) :? I
    , backwards = \(fx :? b) -> case (backwards iso fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR FCanopy, O) -> FR FCanopy
        (FR FCanopy, I) -> FC
    }
  (sn :! SI) SObI _ -> let iso = reindexSucc sn in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FL FBranch -> forwards iso (FR FCanopy) :? O
        FR FCanopy -> forwards iso (FR FCanopy) :? I
        FC -> FBranch
    , backwards = \case
        fx :? b -> case (backwards iso fx, b) of
          (FL fn, b) -> FL (fn :? b)
          (FR FCanopy, O) -> FL FBranch
          (FR FCanopy, I) -> FR FCanopy
        FBranch -> FC
    }
  SObI (_ :! SO) SO -> Iso
    { forwards = \case
        FL FCanopy -> FBranch
        FR (fn :? b) -> fn :? b
    , backwards = \case
        FBranch -> FL FCanopy
        fn :? b -> FR (fn :? b)
    }
  SObI (sm :! SO) SI -> let iso = reindexSucc sm in Iso
    { forwards = \case
        FL FCanopy -> forwards iso (FR FCanopy) :? O
        FR (fn :? b) -> forwards iso (FL fn) :? b
        FC -> forwards iso (FR FCanopy) :? I
    , backwards = \(fx :? b) -> case (backwards iso fx, b) of
        (FR FCanopy, O) -> FL FCanopy
        (FL fn, b) -> FR (fn :? b)
        (FR FCanopy, I) -> FC
    }
  SObI (sm :! SI) _ -> let iso = reindexSucc sm in Iso
    { forwards = \case
        FL FCanopy -> forwards iso (FR FCanopy) :? O
        FR (fm :? b) -> forwards iso (FL fm) :? b
        FR FBranch -> forwards iso (FR FCanopy) :? O
        FC -> FBranch
    , backwards = \case
        fx :? b -> case (backwards iso fx, b) of
          (FL fn, b) -> FR (fn :? b)
          (FR FCanopy, O) -> FR FBranch
          (FR FCanopy, I) -> FL FCanopy
        FBranch -> FC
    }
  (sn :! SO) (sm :! SO) _ -> let iso = reindexAdd sn sm in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FR (fm :? b) -> forwards iso (FR fm) :? b
        FC -> FBranch
    , backwards = \case
        fx :? b -> case backwards iso fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FBranch -> FC
    }
  (sn :! SO) (sm :! SI) SO -> let iso = reindexAdd sn sm in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FR (fm :? b) -> forwards iso (FR fm) :? b
        FR FBranch -> FBranch
    , backwards = \case
        fx :? b -> case backwards iso fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FBranch -> FR FBranch
    }
  (sn :! SO) (sm :! SI) SI -> let iso = reindexAddC sn sm SI in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FR (fm :? b) -> forwards iso (FR fm) :? b
        FR FBranch -> forwards iso FC :? O
        FC -> forwards iso FC :? I
    , backwards = \(fx :? b) -> case (backwards iso fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR fm, b) -> FR (fm :? b)
        (FC, O) -> FR FBranch
        (FC, I) -> FC
    }
  (sn :! SI) (sm :! SO) SO -> let iso = reindexAdd sn sm in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FL FBranch -> FBranch
        FR (fm :? b) -> forwards iso (FR fm) :? b
    , backwards = \case
        fx :? b -> case backwards iso fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FBranch -> FL FBranch
    }
  (sn :! SI) (sm :! SO) SI -> let iso = reindexAddC sn sm SI in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FL FBranch -> forwards iso FC :? O
        FR (fm :? b) -> forwards iso (FR fm) :? b
        FC -> forwards iso FC :? I
    , backwards = \(fx :? b) -> case (backwards iso fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR fm, b) -> FR (fm :? b)
        (FC, O) -> FL FBranch
        (FC, I) -> FC
    }
  (sn :! SI) (sm :! SI) _ -> let iso = reindexAddC sn sm SI in Iso
    { forwards = \case
        FL (fn :? b) -> forwards iso (FL fn) :? b
        FL FBranch -> forwards iso FC :? O
        FR (fm :? b) -> forwards iso (FR fm) :? b
        FR FBranch -> forwards iso FC :? I
        FC -> FBranch
    , backwards = \case
        fx :? b -> case (backwards iso fx, b) of
          (FL fn, b) -> FL (fn :? b)
          (FC, O) -> FL FBranch
          (FR fm, b) -> FR (fm :? b)
          (FC, I) -> FR FBranch
        FBranch -> FC
    }

fuseC :: Tree n a -> Tree m a -> Opt b a -> Tree (AddC n m b) a
fuseC = \cases
  (Canopy a0) (Canopy a1) oa -> Canopy (a0 :* a1) :\ oa
  (taa :\ None) (Canopy a1) None -> taa :\ Some a1
  (taa :\ None) (Canopy a1) (Some a2) -> push taa (a1 :* a2) :\ None
  (taa :\ Some a0) (Canopy a1) oa -> push taa (a0 :* a1) :\ oa
  (Canopy a0) (taa :\ None) None -> taa :\ Some a0
  (Canopy a0) (taa :\ None) (Some a2) -> push taa (a0 :* a2) :\ None
  (Canopy a0) (taa :\ Some a1) oa -> push taa (a1 :* a0) :\ oa
  (taa0 :\ None) (taa1 :\ None) oa -> fuse taa0 taa1 :\ oa
  (taa0 :\ None) (taa1 :\ Some a1) None -> fuse taa0 taa1 :\ Some a1
  (taa0 :\ None) (taa1 :\ Some a1) (Some a2) -> fuseC taa0 taa1 (Some (a1 :* a2)) :\ None
  (taa0 :\ Some a0) (taa1 :\ None) None -> fuse taa0 taa1 :\ Some a0
  (taa0 :\ Some a0) (taa1 :\ None) (Some a2) -> fuseC taa0 taa1 (Some (a0 :* a2)) :\ None
  (taa0 :\ Some a0) (taa1 :\ Some a1) oa -> fuseC taa0 taa1 (Some (a0 :* a1)) :\ oa

fizzC :: SPositive n -> SPositive m -> SBit b -> Tree (AddC n m b) a -> (Tree n a, Tree m a, Opt b a)
fizzC = \cases
  SObI SObI _ (Canopy (a0 :* a1) :\ oa) ->
    (Canopy a0, Canopy a1, oa)
  (_ :! SO) SObI SO (taa :\ Some a1) ->
    (taa :\ None, Canopy a1, None)
  (sn :! SO) SObI SI (taa :\ None) ->
    let (taa0, Canopy (a1 :* a2)) = fizz sn SObI taa
    in (taa0 :\ None, Canopy a1, Some a2)
  (sn :! SI) SObI _ (taa :\ oa) ->
    let (taa0, Canopy (a0 :* a1)) = fizz sn SObI taa
    in (taa0 :\ Some a0, Canopy a1, oa)
  SObI (_ :! SO) SO (taa :\ Some a0) ->
    (Canopy a0, taa :\ None, None)
  SObI (sm :! SO) SI (taa :\ None) ->
    let (taa1, Canopy (a0 :* a2)) = fizz sm SObI taa
    in (Canopy a0,  taa1 :\ None, Some a2)
  SObI (sm :! SI) _ (taa :\ oa) ->
    let (taa1, Canopy (a0 :* a1)) = fizz sm SObI taa
    in (Canopy a0, taa1 :\ Some a1, oa)
  (sn :! SO) (sm :! SO) _ (taa :\ oa) ->
    let (taa0, taa1) = fizz sn sm taa
    in (taa0 :\ None, taa1 :\ None, oa)
  (sn :! SO) (sm :! SI) SO (taa :\ oa) ->
    let (taa0, taa1) = fizz sn sm taa
    in (taa0 :\ None, taa1 :\ oa, None)
  (sn :! SO) (sm :! SI) SI (taa :\ None) ->
    let (taa0, taa1, Some (a1 :* a2)) = fizzC sn sm SI taa
    in (taa0 :\ None, taa1 :\ Some a1, Some a2)
  (sn :! SI) (sm :! SO) SO (taa :\ oa) ->
    let (taa0, taa1) = fizz sn sm taa
    in (taa0 :\ oa, taa1 :\ None, None)
  (sn :! SI) (sm :! SO) SI (taa :\ None) ->
    let (taa0, taa1, Some (a0 :* a2)) = fizzC sn sm SI taa
    in (taa0 :\ Some a0, taa1 :\ None, Some a2)
  (sn :! SI) (sm :! SI) _ (taa :\ oa) ->
    let (taa0, taa1, Some (a0 :* a1)) = fizzC sn sm SI taa
    in (taa0 :\ Some a0, taa1 :\ Some a1, oa)