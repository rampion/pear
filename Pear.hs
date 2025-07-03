{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Indexed types using type-level binary numbers
module Pear where

import Control.Category (Category(..)) -- We don't actually need this for 
                                       -- anything we're doing here, but it 
                                       -- pains me to not implement Category 
                                       -- when the type so clearly supports it
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import Prelude hiding ((.), id)

-- * Bits
-- $

-- | The binary equivalent of digits.
--
-- We'll mainly be using these as types (via DataKinds).
type Bit :: Type
data Bit = O | I -- using O and I for 0 and 1 might be a little cutsy, but I 
                 -- think it helps with legibility
  deriving (Show, Eq, Enum, Ord, Bounded)

-- | A singleton type for bits.
--
-- These give us a way of manipulating type-level bits using value-level logic.
type SBit :: Bit -> Type
data SBit b where
  SO :: SBit O
  SI :: SBit I

deriving instance Show (SBit b)
deriving instance Eq (SBit b)

-- | A constraint that lets us specify a bit value to a function implicitly.
type KnownBit :: Bit -> Constraint
class KnownBit b where
  knownBit :: SBit b

instance KnownBit O where knownBit = SO
instance KnownBit I where knownBit = SI

-- $FBit
-- We could also make an type to represent "the finite values less than the 
-- given bit", 
--
-- @
--    type FBit :: Bit -> Type
--    data FBit b where
--      FO :: FBit I -- O is the only bit value less than I
--      -- FBit O is unpopulated, there are no bit values less than O
-- @
--
-- but it's just as convenient to do without.

-- * Positive
-- $

-- | Positive binary natural numbers, encoded as a sequence of bits
--
--    >>> toEnum @Positive 18
--    ObI :. O :. O :. I :. O
--    >>> toEnum @Positive 19
--    ObI :. O :. O :. I :. I
--    >>> toEnum @Positive 20
--    ObI :. O :. I :. O :. O
type Positive :: Type
data Positive = ObI | Positive :. Bit
  deriving (Eq, Ord) -- it's worth noting that the derived Ord instance 
                     -- automatically does the right thing because the 
                     -- constructors are listed in least-to-most order

infixl 4 :.

-- The derived Show instance for Positive (and the reset of the @infixl@ 
-- operators defined below) would wrap _every_ recursive constructor call in 
-- parentheses, which makes them harder to read.
--
-- This custom instance just removes these parentheses
instance Show Positive where
  showsPrec p = showParen (p >= 4) . fix \loop -> \case
    n :. b -> loop n . showString " :. " . shows b
    ObI -> showString "ObI"

-- |
-- Let's make sure I didn't mess up 'succ'
--
--    >>> succ (ObI :. O :. O :. I :. O)
--    ObI :. O :. O :. I :. I
--    >>> succ (ObI :. O :. O :. I :. I)
--    ObI :. O :. I :. O :. O
--
-- or 'fromEnum'
--
--    >>> fromEnum (ObI :. O :. O :. I :. O)
--    18
--    >>> fromEnum (ObI :. O :. O :. I :. I)
--    19
--    >>> fromEnum (ObI :. O :. I :. O :. O)
--    20
instance Enum Positive where
  succ = \case
    n :. I -> succ n :. O
    n :. O -> n :. I
    ObI -> ObI :. O

  pred =
    fromMaybe (error "Positive.pred :: Invalid input") .
    predPositive

  toEnum = 
    fromMaybe (error "Positive.toEnum :: Invalid input") .
    toPositive

  fromEnum = \case
    ObI -> 1
    n :. b -> 2 * fromEnum n + fromEnum b


instance Bounded Positive where
  minBound = ObI
  maxBound = fix (:. I) -- infinity!

-- | A non-partial alternative to 'pred'.
--
--    >>> predPositive $ ObI :. O :. O :. I
--    Just (ObI :. O :. O :. O)
--    >>> predPositive $ ObI :. O :. O :. O
--    Just (ObI :. I :. I)
--
-- Not all Positive numbers have a Positive predecessor (looking at you 'ObI').
--
--    >>> predPositive ObI
--    Nothing
predPositive :: Positive -> Maybe Positive
predPositive = loop Nothing Just where
  loop fail done  = \case
    n :. O -> loop (done ObI) (done . (:. I)) n
    n :. I -> done (n :. O)
    ObI -> fail

-- | A non-partial alternative to 'toEnum'.
--
--    >>> toPositive 11
--    Just (ObI :. O :. I :. I)
--    >>> toPositive 87
--    Just (ObI :. O :. I :. O :. I :. I :. I)
--
-- Not all integers are positive numbers.
--
--    >>> toPositive 0
--    Nothing
--    >>> toPositive (-99)
--    Nothing
toPositive :: Int -> Maybe Positive
toPositive = \case
  n | n <= 0 -> Nothing
    | otherwise -> Just $ loop n
  where
    loop = \case
      1 -> ObI
      n -> 
        let (q, r) = n `quotRem` 2
        in loop q :. toEnum r

-- | A singleton type for positive integers
type SPositive :: Positive -> Type
data SPositive n where
  SObI :: SPositive ObI
  (:!) ::  SPositive n -> SBit b -> SPositive (n :. b)

infixl 4 :!

deriving instance Eq (SPositive n)

-- same song-and-dance for avoiding parens when 'show'ing
-- 'SPositive' numbers. 
--
-- However, I'm very disappointed to say, doesn't apply when asking @ghci@ to 
-- print a type
--
--    >>> :t SObI :! SO :! SO :! SO
--    SObI :! SO :! SO :! SO :: SPositive (((ObI :. O) :. O) :. O)
instance Show (SPositive n) where
  showsPrec p = showParen (p >= 4) . fix1 \loop -> \case
    sn :! sb -> loop sn . showString " :! " . shows sb
    SObI -> showString "SObI"

-- | Convert a 'SPositive' into its equivalent integer value
--
--    >>> fromSPositive $ SObI :! SO :! SO :! SO
--    8
fromSPositive :: SPositive n -> Int
fromSPositive = loop 1 0 where
  loop :: Int -> Int -> SPositive n -> Int
  loop !p !t = \case
    SObI -> p + t
    sn :! sb -> loop (2*p) (t + case sb of SO -> 0 ; SI -> p) sn

-- | Convert an 'Positive' value into an 'SPositive' and pass it to a 
-- continuation
--
--    >>> withSPositive show (ObI :. O :. O :. I :. I)
--    "SObI :! SO :! SO :! SI :! SI"
--    >>> withSPositive fromSPositive (ObI :. O :. O :. I :. I)
--    19
--
-- This will be a needed tool if we ever want to do something like build a tree 
-- of a runtime-specified size.
withSPositive :: (forall n. SPositive n -> r) -> Positive -> r
-- the continuation trick is necessary because @Positive -> SPositive n@ would 
-- mean that the caller gets to specify the type @n@ at compile time, while we 
-- actually want to be able to have the input determine the type at runtime.
withSPositive k = \case
  ObI -> k SObI
  bs :. O -> withSPositive (k . (:! SO)) bs
  bs :. I -> withSPositive (k . (:! SI)) bs

-- | A constraint that lets us specify a positive value to a function 
-- implicitly
type KnownPositive :: Positive -> Constraint
class KnownPositive n where
  knownPositive :: SPositive n

instance KnownPositive ObI where
  knownPositive = SObI

instance (KnownPositive n, KnownBit b) => KnownPositive (n :. b) where
  knownPositive = knownPositive @n :! knownBit @b

-- | Specify what value of n should be inferred for the given block.
withKnownPositive :: ((KnownPositive n) => r) -> SPositive n -> r
withKnownPositive r = \case
  SObI -> r
  sn :! SO -> withKnownPositive r sn
  sn :! SI -> withKnownPositive r sn

-- | The finite set of naturals strictly less than the specified index.
--
-- Consider the binary expansion of a positive number:
--
-- @
--    0b  1  0  1  1  0  1      
--
--       32 16  8  4  2  1  each bit position corresponds to a power of 2
--
--       32  0  8  4  0  1  but only the nonzero bits contribute to the value
-- @
--
-- In particular, all the natural numbers *less* than a positive number share
-- some prefix of the positive number's binary expansion up to some bit 
-- position, where the natural number has a 0 but the positive number has a 1.
--
-- @
--    0b    1    0    1    1    0    1  = 45
--
--          1    0    1    1    0   [0] = 44
--
--          1    0    1   [0]   1    0  = 42
--
--          1    0   [0]   1    0    1  = 39
--
--         [0]   0    1    1    1    1  = 15
--
--         [0]   0    0    0    0    0  = 0
-- @
--
-- 'FPositive' partitions the naturals less than its index into groups by
-- their maximum shared prefix with the index. Within those groups, each
-- natural is associated with its unique suffix.
--
-- @
--    0b    1    0    1    1    0    1  = 45
--
--    0b    1    0    1    1    0   [0]
--                                      = 44
--
--    0b    1    0    1   [0]   _    _
--                              1    1  = 43
--                              1    0  = 42
--                              0    1  = 41
--                              0    0  = 40
--
--    0b    1    0   [0]   _    _    _
--                         1    1    1  = 39
--                         1    1    0  = 38
--                         1    0    1  = 37
--                         ...  
--                         0    0    0  = 32
--
--    0b   [0]   _    _    _    _    _
--               1    1    1    1    1  = 31
--               1    1    1    1    0  = 30
--               1    1    1    0    1  = 29
--               ...  
--               0    0    0    0    1  = 1
--               0    0    0    0    0  = 0
-- @
--
-- If the prefix is empty (the most significant set bit of the index is unset 
-- in the natural), then the suffix is attached to the 'FCanopy' constructor.
--
-- If the prefix is non-empty, then the suffix is attached to the 'FBranch' 
-- constructor.
--
-- (The names are derived from their use as offsets in 'Tree')
--
-- @
--    0b    1    0    1    1    0    1  = 45
--
--    0b    1    0    1    1    0   [0]
--                             FBranch  = 44
--
--    0b    1    0    1   [0]   _    _
--                   FBranch :? I :? I  = 43
--                   FBranch :? I :? O  = 42
--                   FBranch :? O :? I  = 41
--                   FBranch :? O :? O  = 40
--
--    0b    1    0   [0]   _    _    _
--              FBranch :? I :? I :? I  = 39
--              FBranch :? I :? I :? O  = 38
--              FBranch :? I :? O :? I  = 37
--                         ...
--              FBranch :? O :? O :? O  = 32
--
--    0b   [0]   _    _    _    _    _
--    FCanopy :? I :? I :? I :? I :? I  = 31
--    FCanopy :? I :? I :? I :? I :? O  = 30
--    FCanopy :? I :? I :? I :? O :? I  = 29
--               ...
--    FCanopy :? O :? O :? O :? O :? I  = 1
--    FCanopy :? O :? O :? O :? O :? O  = 0
-- @
--
-- Note that this means that we need to know the type of an 'FPositive' value 
-- in order to be able to know its integer value.
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
safeToFPositive i = positiveToFPositive =<< toPositive i
  
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
    Canopy a -> showString "Canopy " . showsPrec 10 a

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

-- * Miscellaneous Utilities
-- $

-- ** 'fix1' and 'fix2'
--
-- $
-- 'fix1' and 'fix2' are really just the normal 'Data.Function.fix' but with 
-- more specific types.
--
--  * @fix1 = fix \@(forall i. f i -> a)@
--  * @fix2 = fix \@(forall i x. c x => f i x -> a)@
--
-- However you'll need to enable @ImpredicativeTypes@ for that, and I'm not 
-- cosy with that particular language extension yet.
--
-- Also I thought the long type applications would be a little distracting, 
-- didactically.
--
-- So I made some helpers.  I'm still not 100% on them, so I might just switch 
-- back to defining local helpers instead.

-- | Like 'fix', but allowing for a changing final type parameter
fix1 :: forall f a. ((forall i. f i -> a) -> (forall i. f i -> a)) -> (forall i. f i -> a)
fix1 f = g where
  g :: forall i. f i -> a
  g = f g

-- | Like 'fix', but allowing for two changing type parameters
fix2 :: forall c f a . ((forall i x. c x => f i x -> a) -> (forall i x. c x => f i x -> a)) -> (forall i x. c x => f i x -> a)
fix2 f = g where
  g :: forall i x. c x => f i x -> a
  g = f g

-- ** Lens
-- $lens
-- The great thing about van Laarhoven lenses is that you don't need to import 
-- a library to define them.
--
-- But an appropriately named type synonym certainly cuts down on the noise.

-- | [van Laarhoven lenses](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html) 
-- that don't allow updates to change the type
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
