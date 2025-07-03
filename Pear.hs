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
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import Prelude hiding ((.), id)

-- * Bits
-- $
-- We'll need a number of ways to represent bits at the value, type, and constraint level.

-- ** @Bit@
-- $

-- | The binary equivalent of digits.
--
-- We'll mainly be using these as types (via DataKinds).
type Bit :: Type
data Bit = O | I -- using O and I for 0 and 1 might be a little cutsy, but I 
                 -- think it helps with legibility
  deriving (Show, Eq, Enum, Ord, Bounded)

-- ** @SBit@
-- $

-- | A singleton type for bits.
--
-- These give us a way of manipulating type-level bits using value-level logic.
type SBit :: Bit -> Type
data SBit b where
  SO :: SBit O
  SI :: SBit I

deriving instance Show (SBit b)
deriving instance Eq (SBit b)

-- ** The @KnownBit@ class
-- $

-- | A constraint that lets us specify a bit value to a function implicitly.
type KnownBit :: Bit -> Constraint
class KnownBit b where
  knownBit :: SBit b

instance KnownBit O where knownBit = SO
instance KnownBit I where knownBit = SI

-- ** The missing "@FBit@"
-- $FBit
--
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

-- * Positive numbers
-- $
-- We'll need a number of ways to represent positive numbers at the value, type, and constraint level.

-- ** @Positive@
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
  deriving (Eq)

-- | The derived Ord instance works because the constructors were listed in 
-- least-to-most order
--
--    >>> ObI < (ObI :. O)
--    True
--    >>> maximum [ObI :. O, ObI, ObI :. I :. I, ObI :. O :. I]
--    ObI :. I :. I
deriving instance Ord (Positive)

infixl 4 :.

-- The derived Show instance for Positive (and the reset of the @infixl@ 
-- operators defined below) would wrap _every_ recursive constructor call in 
-- parentheses, which makes them harder to read.
--
-- This custom instance just removes these parentheses, only keeping any
-- parentheses around the entire Positive value (if necessary)
--
--    >>> Just ObI
--    Just ObI
--    >>> Just (ObI :. I :. O :. I)
--    Just (ObI :. I :. O :. I)
--
instance Show Positive where
  showsPrec p = \case
    n :. b -> showParen (p >= 4) do
      shows n . showString " :. " . shows b
    ObI -> showString "ObI"

-- | Let's make sure I didn't mess up 'succ'
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
  maxBound = maxBound :. I -- infinity!

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

-- ** @SPositive@
-- $

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
  showsPrec p = \case
    sn :! sb -> showParen (p >= 4) do
      shows sn . showString " :! " . shows sb
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

-- ** @KnownPositive@
-- $

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

-- ** @FPositive@
-- $

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
  FCanopy :: FPositive ObI
  (:?) :: FPositive n -> Bit -> FPositive (n :. b)
  FBranch :: FPositive (n :. I)

deriving instance Eq (FPositive n)
-- | The derived Ord instance works because the constructors were listed in 
-- least-to-most order
--
--    >>> (FCanopy :? O :? I :? I) `min` (FCanopy :? O :? I :? O)
--    FCanopy :? O :? I :? O
--    >>> (FCanopy :? O :? I :? I) `min` (FBranch :? O)
--    FCanopy :? O :? I :? I
--    >>> (FBranch :? I) `min` (FBranch :? O)
--    FBranch :? O
deriving instance Ord (FPositive n)

instance Show (FPositive n) where
  showsPrec p = \case
    fn :? b -> showParen (p >= 4) do
      shows fn . showString " :? " . shows b
    FCanopy -> showString "FCanopy"
    FBranch -> showString "FBranch"

instance KnownPositive n => Bounded (FPositive n) where
  minBound = minBoundFPositive' knownPositive
  maxBound = maxBoundFPositive' knownPositive

-- | Alternative to 'minBound', where the value of @n@ is given explicitly.
--
-- This gives the encoding for 0 in FPositive n.
--
--    >>> minBoundFPositive' $ SObI
--    FCanopy
--    >>> minBoundFPositive' $ SObI :! SO :! SI :! SO
--    FCanopy :? O :? O :? O
minBoundFPositive' :: SPositive n -> FPositive n
minBoundFPositive' = \case
  SObI -> FCanopy
  sn :! _ -> minBoundFPositive' sn :? O

-- | Alternative to 'maxBound', where the value of @n@ is given explicitly.
--
-- This gives the encoding for n - 1 in FPositive n.
--
--    >>> maxBoundFPositive' $ SObI
--    FCanopy
--    >>> maxBoundFPositive' $ SObI :! SO :! SI :! SO
--    FBranch :? I
--    >>> maxBoundFPositive' $ SObI :! SO :! SI :! SI
--    FBranch
maxBoundFPositive' :: SPositive n -> FPositive n
maxBoundFPositive' = \case
  SObI -> FCanopy
  sn :! SO -> maxBoundFPositive' sn :? I
  _ :! SI -> FBranch

instance KnownPositive n => Enum (FPositive n) where
  succ =
    fromMaybe (error "FPositive.succ :: Invalid input") .
    succFPositive

  pred =
    fromMaybe (error "FPositive.pred :: Invalid input") .
    predFPositive
  
  toEnum = 
    fromMaybe (error "FPositive.toEnum :: Invalid input") .
    toFPositive

  fromEnum = fromFPositive' knownPositive

-- | A non-partial alternative to 'succ'
--
--    >>> succFPositive @(ObI :. I) $ FCanopy :? O
--    Just (FCanopy :? I)
--    >>> succFPositive @(ObI :. I) $ FCanopy :? I
--    Just FBranch
--
-- Not all 'FPositive n' numbers have a successor less than n (that is, 
-- 'maxBound' does not).
--
--    >>> succFPositive @(ObI :. I) $ FBranch
--    Nothing
succFPositive :: KnownPositive n => FPositive n -> Maybe (FPositive n)
succFPositive = succFPositive' knownPositive

-- | Alternative to 'succFPositive', where the value of @n@ is given 
-- explicilty.
--
--    >>> succFPositive' (SObI :! SI) (FCanopy :? O)
--    Just (FCanopy :? I)
--    >>> succFPositive' (SObI :! SI) (FCanopy :? I)
--    Just FBranch
--    >>> succFPositive' (SObI :! SI) FBranch
--    Nothing
succFPositive' :: SPositive n -> FPositive n -> Maybe (FPositive n)
succFPositive' = loop Nothing Just where
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

-- | A non-partial alternative to 'pred'
--
--    >>> predFPositive @(ObI :. O :. O :. I :. I) FBranch
--    Just (FBranch :? I)
--    >>> predFPositive @(ObI :. O :. O :. I :. I) (FBranch :? I)
--    Just (FBranch :? O)
--    >>> predFPositive @(ObI :. O :. O :. I :. I) (FBranch :? O)
--    Just (FCanopy :? I :? I :? I :? I)
predFPositive :: KnownPositive n => FPositive n -> Maybe (FPositive n)
predFPositive = predFPositive' knownPositive

-- | Alternative to 'predFPositive', where the value of @n@ is given
--
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) FBranch
--    Just (FBranch :? I)
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) (FBranch :? I)
--    Just (FBranch :? O)
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) (FBranch :? O)
--    Just (FCanopy :? I :? I :? I :? I)
predFPositive' :: SPositive n -> FPositive n -> Maybe (FPositive n)
predFPositive' = loop id where
  loop :: (FPositive n -> r) -> SPositive n -> FPositive n -> Maybe r
  loop k = flip \cases
    FCanopy -> \SObI -> Nothing
    FBranch -> \(sn :! SI) -> Just $ k $ maxBoundFPositive' sn :? I 
    (fn :? I) -> \_ -> Just $ k (fn :? O) 
    (fn :? O) -> \(sn :! _) -> loop (k . (:? I)) sn fn

-- | A non-partial alternative to 'toEnum'
--
--    >>> toFPositive @(ObI :. O :. O :. O) 5
--    Just (FCanopy :? I :? O :? I)
--    >>> toFPositive @(ObI :. O :. O :. O) 0
--    Just (FCanopy :? O :? O :? O)
--
--  Not all integers are positive numbers less than n
--
--    >>> toFPositive @(ObI :. O :. O :. O) 10
--    Nothing
--    >>> toFPositive @(ObI :. O :. O :. O) (-5)
--    Nothing
toFPositive :: KnownPositive n => Int -> Maybe (FPositive n)
toFPositive 0 = Just minBound
toFPositive i = positiveToFPositive =<< toPositive i
  
-- | Convert a given 'Positive' value to an @'FPositive' n@ for some known @n@.
--
--    >>> positiveToFPositive @(ObI :. O :. O :. O) (ObI :. O :. I)
--    Just (FCanopy :? I :? O :? I)
--    >>> positiveToFPositive @(ObI :. I :. O) (ObI :. O :. I)
--    Just (FBranch :? I)
--
--  Not all positive integers are less than @n@.
--
--    >>> positiveToFPositive @(ObI :. O) (ObI :. O :. I)
--    Nothing
positiveToFPositive :: KnownPositive n => Positive -> Maybe (FPositive n)
positiveToFPositive = positiveToFPositive' knownPositive

-- | Alternative to 'positiveToFPositive', where the value of @n@ is given 
-- explicilty.
--
--    >>> positiveToFPositive' (SObI :! SO :! SO :! SO) (ObI :. O :. I)
--    Just (FCanopy :? I :? O :? I)
--    >>> positiveToFPositive' (SObI :! SI :! SO) (ObI :. O :. I)
--    Just (FBranch :? I)
--
--  Not all positive integers are less than @n@.
--
--    >>> positiveToFPositive' (SObI :! SO) (ObI :. O :. I)
--    Nothing
positiveToFPositive' :: SPositive n -> Positive -> Maybe (FPositive n)
positiveToFPositive' = loop Nothing id where
  loop :: Maybe r -> (FPositive n -> r) -> SPositive n -> Positive -> Maybe r
  loop eq lt = \cases
    SObI ObI -> eq
    SObI _   -> Nothing
    (sn :! _) ObI -> Just $ lt $ minBoundFPositive' sn :? I
    (sn :! SO) (m :. O) -> loop eq (lt . (:? O)) sn m
    (sn :! SO) (m :. I) -> loop Nothing (lt . (:? I)) sn m
    (sn :! SI) (m :. O) -> loop (Just $ lt FBranch) (lt . (:? O)) sn m
    (sn :! SI) (m :. I) -> loop eq (lt . (:? I)) sn m

-- | Alternative to 'fromEnum', where the value of @n@ is given explicitly.
--
--    >>> fromFPositive' (SObI :! SO :! SO :! SO) (FCanopy :? I :? O :? I)
--    5
--    >>> fromFPositive' (SObI :! SI :! SO) (FBranch :? I)
--    5
fromFPositive' :: SPositive n -> FPositive n -> Int
fromFPositive' = loop 1 0 where
  loop :: Int -> Int -> SPositive n -> FPositive n -> Int
  loop !p !t = \cases
    SObI FCanopy -> t
    (sn :! _) FBranch -> fromSPositive sn * 2 * p + t
    (sn :! _) (fn :? b) -> loop (2*p) (t + p * fromEnum b) sn fn

-- * Containers
-- $
-- Using indexes to track how many elements are in a container

-- ** @Opt@
-- $

-- | An indexed variant of 'Maybe', exposing whether a value of type @a@ is contained.
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

-- | Alternative to 'liftA2' without the 'KnownBit' constraint.
liftO2 :: (u -> v -> w) -> Opt b u -> Opt b v -> Opt b w
liftO2 f = \cases
  None None -> None 
  (Some u) (Some v) -> Some (f u v)

-- | Count the number of elements in an 'Opt' as a 'SBit'.
optSize :: Opt b a -> SBit b
optSize = \case
  None -> SO
  Some _ -> SI

-- ** @Pair@
-- $

-- | A variant of '(,)' where both elements have the same type.
type Pair :: Type -> Type
data Pair a = a :* a
  deriving (Functor, Foldable, Traversable, Eq, Show)

infix 8 :*

instance Applicative Pair where
  (f0 :* f1) <*> (a0 :* a1) = f0 a0 :* f1 a1
  pure a = a :* a

-- ** @Tree@
-- $

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
  showsPrec p = \case
    taa :\ oa -> showParen (p >= 4) do
      shows taa . showString " :\\ " . shows oa
    Canopy a -> showString "Canopy " . showsPrec 10 a

instance KnownPositive n => Applicative (Tree n) where
  liftA2 = liftT2
  pure a = generate (const a)

-- | Alternative to 'liftA2' without the 'KnownPositive' constraint.
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

-- * Lenses
-- $
-- It's convenient to have lenses for the 'Tree', 'Opt', and 'Pair' data 
-- structures in order to look up and modify values.
-- 
-- The great thing about [van Laarhoven lenses](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html)  is that you don't need to import 
-- a library to define them.
--
-- But an appropriately named type synonym certainly cuts down on the noise.

-- | A classic van Laarhoven lens
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A van Laarhoven lens that doesn't allow updates to change the type
type Lens' s a = Lens s s a a

-- $setup
-- Since we're not using a lens library, we'll need some tools for the 
-- examples.
--
--    >>> :set -XDataKinds
--    >>> import Data.Functor.Identity
--    >>> import Data.Functor.Const
--    >>> import Data.Function ((&))
--    >>> :{
--      view :: Lens' s a -> s -> a
--      view l = getConst . l Const
--    :}
--
--    >>> :{
--      over :: Lens s t a b -> (a -> b) -> s -> t
--      over l f = runIdentity . l (Identity . f)
--    :}
--
--    >>> :{
--      set :: Lens s t a b -> b -> s -> t
--      set l = over l . const
--    :}

-- | A lens for accessing the first element of a 'Pair'.
--
--    >>> ('a' :* 'b') & view _O
--    'a'
--    >>> ('a' :* 'b') & set _O 'A'
--    'A' :* 'b'
_O :: Lens' (Pair a) a
_O f (a0 :* a1) = f a0 <&> (:* a1)

-- | A lens for accessing the second element of a 'Pair'.
--
--    >>> ('a' :* 'b') & view _I
--    'b'
--    >>> ('a' :* 'b') & set _I 'B'
--    'a' :* 'B'
_I :: Lens' (Pair a) a
_I f (a0 :* a1) = (a0 :*) <$> f a1

-- | A lens for accessing an arbitrary element of a 'Pair', using a 'Bit' as an 
-- index
--
--    >>> (100 :* 100) & over (atPair O) succ
--    101 :* 100
--    >>> (100 :* 100) & over (atPair I) succ
--    100 :* 101
atPair :: Bit -> Lens' (Pair a) a
atPair = \case
  O -> _O
  I -> _I

-- | A lens for accessing the value held by an @'Opt' 'I'@
--
--    >>> (Some 'a') & view _Some
--    'a'
--    >>> (Some 'a') & set _Some "hello"
--    Some "hello"
_Some :: Lens (Opt I a) (Opt I b) a b
_Some f (Some a) = Some <$> f a

-- | A lens for accessing the value held by a @'Tree' 'ObI'@
--
--    >>> (Canopy 'a') & view _Canopy
--    'a'
_Canopy :: Lens (Tree ObI a) (Tree ObI b) a b
_Canopy f (Canopy a) = Canopy <$> f a

-- | A lens for accessing the first argument of ':\' in a 'Tree'.
--
--    >>> let t = Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None 
--    >>> t & view (_Up . _Up . _Canopy . _I . _O)
--    'c'
--    >>> t & set (_Up . _Up . _Canopy . _I . _O) 'C'
--    Canopy (('a' :* 'b') :* ('C' :* 'd')) :\ None :\ None 
_Up :: Lens (Tree (n :. b) a) (Tree (m :. b) a) (Tree n (Pair a)) (Tree m (Pair a))
_Up f (taa :\ oa) = f taa <&> (:\ oa)

-- | A lens for accessing the second argument of ':\' in a 'Tree'
--
--    >>> let t = Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None 
--    >>> t & view (_Up . _Off)
--    None
--    >>> let t' = t & set (_Up . _Off) (Some ('e' :* 'f'))
--    >>> t'
--    Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('e' :* 'f') :\ None 
--    >>> t' & view (_Up . _Off . _Some . _O)
--    'e'
_Off :: Lens (Tree (n :. b) a) (Tree (n :. c) a) (Opt b a) (Opt c a)
_Off f (taa :\ oa) = (taa :\) <$> f oa

-- | A lens for accessing the last element of a @'Tree' (n :. I)@
--
--    >>> let t = Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ Some 'e'
--    >>> t & view _Branch
--    'e'
--    >>> t & set _Branch 'E'
--    Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ Some 'E'
_Branch :: Lens' (Tree (n :. I) a) a
_Branch = _Off . _Some

-- | A lens for accessing an arbitrary element of a 'Tree', using 'FPositive' as an index.
--
--    >>> let t = Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ Some 'e'
--    >>> t & view (atTree (FCanopy :? O :? O))
--    'a'
--    >>> t & view (atTree (FCanopy :? I :? O))
--    'c'
--    >>> t & view (atTree FBranch)
--    'e'
--    >>> t & set (atTree (FCanopy :? O :? I)) 'B'
--    Canopy (('a' :* 'B') :* ('c' :* 'd')) :\ None :\ Some 'e'
atTree :: FPositive n -> Lens' (Tree n a) a
atTree = \case
  FCanopy -> _Canopy
  FBranch -> _Branch
  fn :? b -> _Up . atTree fn . atPair b

-- * Misc
-- $

type Iso :: Type -> Type -> Type
data Iso a b = Iso { forwards :: a -> b, backwards :: b -> a }

instance Category Iso where
  id = Iso id id
  Iso f0 b0 . Iso f1 b1 = Iso (f0 . f1) (b1 . b0)
