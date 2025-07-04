{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Indexed types using type-level binary numbers
module Pear where
-- The examples in the documentation were written to be checked via doctest 
-- <https://github.com/sol/doctest>
--
-- Simply run @doctest Pear.hs@ to make sure they all compile and run as 
-- expected.

import Control.Category (Category(..)) -- We don't actually need this for 
                                       -- anything we're doing here, but it 
                                       -- pains me to not implement Category 
                                       -- when the type so clearly supports it
import Data.Foldable1 (Foldable1(foldMap1))
import Data.Functor ((<&>))
import Data.Functor.Const (Const(..))
import Data.Kind (Type, Constraint)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import Prelude hiding ((.), id, lookup)

-- * Notation
-- $
-- There's a lot of variants on bits and numbers in this module. To help keep 
-- them all straight, I use hungarian notation.
--
--  - the @S@ prefix is used for singleton types and constructors ('SBit', 
--    'SO', 'SI', 'SPositive')
--
--  - the @F@ prefix for finite types and constructors ('FPositive', 'FObO', 
--    'FPrefixO', 'FSucc', 'FAdd', 'FAddC', 'FL', 'FR', 'FC').
--
--  - the @C@ suffix is used when a type or function handles carry bits 
--    ('AddC', 'fuseC', 'fizzC', etc.)
--
-- There's a number of functions that have two variants - one that has a 
-- `KnownPositive` constraint and another that instead has a 'SPositive' 
-- argument.  The latter is always marked by a prime symbol (e.g. 'succFPositive' 
-- vs 'succFPositive'', 'generate' vs 'generate'', etc).
--
-- There's also a lot of types that use operators. This is because I believe 
-- they help keep the focus onto the more relevant things, like the bit 
-- constructors or the values.
--
--  - 'Positive', 'SPositive', and 'FPositive' use ':.', ':!', and ':?' 
--    respectively. As a mnemonic, note that all use punctuation that 
--    terminates a sentence.
--
--  - 'Pair' uses ':*', since it's a product type.
--
--  - 'Tree' uses ':\' for branches coming off the main trunk of the tree. See 'Tree' for
--    how this works as a visual metaphor.

-- * Bits
-- $
-- We'll need a few ways to represent bits at the value, type, and constraint level.

-- ** @Bit@
-- $

-- | The binary equivalent of digits.
--
-- We'll mainly be using these as types (via DataKinds).
type Bit :: Type
data Bit = O | I -- using O and I for 0 and 1 might be a little cutesy, but I 
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
-- type FBit :: Bit -> Type
-- data FBit b where
--   FO :: FBit I -- O is the only bit value less than I
--   -- FBit O is unpopulated, there are no bit values less than O
-- @
--
-- but it's just as convenient to do without.

-- * Positive numbers
-- $
-- We'll need a few ways to represent positive numbers at the value, type, and constraint level.

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

-- | The finite set of naturals strictly less than a specified value.
--
-- Consider the binary expansion of a positive number:
--
-- @
-- 0b  1  0  1  1  0  1      
--
--    32 16  8  4  2  1  each bit position corresponds to a power of 2
--
--    32  0  8  4  0  1  but only the nonzero bits contribute to the value
-- @
--
-- In particular, all the natural numbers *less* than a positive number share
-- some prefix of the positive number's binary expansion up to some bit 
-- position, where the natural number has a 0 but the positive number has a 1.
--
-- @
-- 0b    1    0    1    1    0    1  = 45
--
--       1    0    1    1    0   [0] = 44
--
--       1    0    1   [0]   1    0  = 42
--
--       1    0   [0]   1    0    1  = 39
--
--      [0]   0    1    1    1    1  = 15
--
--      [0]   0    0    0    0    0  = 0
-- @
--
-- @'FPositive' n@ partitions the naturals less than @n@ into groups by
-- their maximum shared prefix with @n@. Within those groups, each
-- natural is associated with its unique suffix.
--
-- @
-- 0b    1    0    1    1    0    1  = 45
--
-- 0b    1    0    1    1    0   [0]
--                                   = 44
--
-- 0b    1    0    1   [0]   _    _
--                           1    1  = 43
--                           1    0  = 42
--                           0    1  = 41
--                           0    0  = 40
--
-- 0b    1    0   [0]   _    _    _
--                      1    1    1  = 39
--                      1    1    0  = 38
--                      1    0    1  = 37
--                      ...  
--                      0    0    0  = 32
--
-- 0b   [0]   _    _    _    _    _
--            1    1    1    1    1  = 31
--            1    1    1    1    0  = 30
--            1    1    1    0    1  = 29
--            ...  
--            0    0    0    0    1  = 1
--            0    0    0    0    0  = 0
-- @
--
-- If the prefix is empty (the most significant set bit of @n@ is unset 
-- in the natural), then the suffix is attached to the 'FObO' constructor.
--
-- If the prefix is non-empty, then the suffix is attached to the 'FPrefixO' 
-- constructor.
--
-- @
-- 0b    1    0    1    1    0    1  = 45
--
-- 0b    1    0    1    1    0   [0]
--                         FPrefixO  = 44
--
-- 0b    1    0    1   [0]   _    _
--               FPrefixO :? I :? I  = 43
--               FPrefixO :? I :? O  = 42
--               FPrefixO :? O :? I  = 41
--               FPrefixO :? O :? O  = 40
--
-- 0b    1    0   [0]   _    _    _
--          FPrefixO :? I :? I :? I  = 39
--          FPrefixO :? I :? I :? O  = 38
--          FPrefixO :? I :? O :? I  = 37
--                      ...
--          FPrefixO :? O :? O :? O  = 32
--
-- 0b   [0]   _    _    _    _    _
--    FObO :? I :? I :? I :? I :? I  = 31
--    FObO :? I :? I :? I :? I :? O  = 30
--    FObO :? I :? I :? I :? O :? I  = 29
--            ...
--    FObO :? O :? O :? O :? O :? I  =  1
--    FObO :? O :? O :? O :? O :? O  =  0
-- @
--
-- Note that this means that we need to know the type of an 'FPositive' value 
-- in order to be able to know its integer value.
type FPositive :: Positive -> Type
data FPositive n where
  FObO :: FPositive ObI
  (:?) :: FPositive n -> Bit -> FPositive (n :. b)
  FPrefixO :: FPositive (n :. I)

deriving instance Eq (FPositive n)
-- | The derived Ord instance works because the constructors were listed in 
-- least-to-most order
--
--    >>> (FObO :? O :? I :? I) `min` (FObO :? O :? I :? O)
--    FObO :? O :? I :? O
--    >>> (FObO :? O :? I :? I) `min` (FPrefixO :? O)
--    FObO :? O :? I :? I
--    >>> (FPrefixO :? I) `min` (FPrefixO :? O)
--    FPrefixO :? O
deriving instance Ord (FPositive n)

instance Show (FPositive n) where
  showsPrec p = \case
    fn :? b -> showParen (p >= 4) do
      shows fn . showString " :? " . shows b
    FObO -> showString "FObO"
    FPrefixO -> showString "FPrefixO"

instance KnownPositive n => Bounded (FPositive n) where
  minBound = minBoundFPositive' knownPositive
  maxBound = maxBoundFPositive' knownPositive

-- | Alternative to 'minBound', where the value of @n@ is given explicitly.
--
-- This gives the encoding for 0 in FPositive n.
--
--    >>> minBoundFPositive' $ SObI
--    FObO
--    >>> minBoundFPositive' $ SObI :! SO :! SI :! SO
--    FObO :? O :? O :? O
minBoundFPositive' :: SPositive n -> FPositive n
minBoundFPositive' = \case
  SObI -> FObO
  sn :! _ -> minBoundFPositive' sn :? O

-- | Alternative to 'maxBound', where the value of @n@ is given explicitly.
--
-- This gives the encoding for n - 1 in FPositive n.
--
--    >>> maxBoundFPositive' $ SObI
--    FObO
--    >>> maxBoundFPositive' $ SObI :! SO :! SI :! SO
--    FPrefixO :? I
--    >>> maxBoundFPositive' $ SObI :! SO :! SI :! SI
--    FPrefixO
maxBoundFPositive' :: SPositive n -> FPositive n
maxBoundFPositive' = \case
  SObI -> FObO
  sn :! SO -> maxBoundFPositive' sn :? I
  _ :! SI -> FPrefixO

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
--    >>> succFPositive @(ObI :. I) $ FObO :? O
--    Just (FObO :? I)
--    >>> succFPositive @(ObI :. I) $ FObO :? I
--    Just FPrefixO
--
-- Not all 'FPositive n' numbers have a successor less than n (that is, 
-- 'maxBound' does not).
--
--    >>> succFPositive @(ObI :. I) $ FPrefixO
--    Nothing
succFPositive :: KnownPositive n => FPositive n -> Maybe (FPositive n)
succFPositive = succFPositive' knownPositive

-- | Alternative to 'succFPositive', where the value of @n@ is given 
-- explicilty.
--
--    >>> succFPositive' (SObI :! SI) (FObO :? O)
--    Just (FObO :? I)
--    >>> succFPositive' (SObI :! SI) (FObO :? I)
--    Just FPrefixO
--    >>> succFPositive' (SObI :! SI) FPrefixO
--    Nothing
succFPositive' :: SPositive n -> FPositive n -> Maybe (FPositive n)
succFPositive' = loop Nothing Just where
  loop :: r -> (FPositive n -> r) -> SPositive n -> FPositive n -> r
  loop failure success = flip \case
    FObO -> \SObI -> failure
    FPrefixO -> \(_ :! SI) -> failure
    fn :? O -> \_ -> success (fn :? I)
    fn :? I -> \(sn :! sb) ->
      let failure' = case sb of
            SO -> failure
            SI -> success FPrefixO
      in
      loop failure' (success . (:? O)) sn fn

-- | A non-partial alternative to 'pred'
--
--    >>> predFPositive @(ObI :. O :. O :. I :. I) FPrefixO
--    Just (FPrefixO :? I)
--    >>> predFPositive @(ObI :. O :. O :. I :. I) (FPrefixO :? I)
--    Just (FPrefixO :? O)
--    >>> predFPositive @(ObI :. O :. O :. I :. I) (FPrefixO :? O)
--    Just (FObO :? I :? I :? I :? I)
predFPositive :: KnownPositive n => FPositive n -> Maybe (FPositive n)
predFPositive = predFPositive' knownPositive

-- | Alternative to 'predFPositive', where the value of @n@ is given
--
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) FPrefixO
--    Just (FPrefixO :? I)
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) (FPrefixO :? I)
--    Just (FPrefixO :? O)
--    >>> predFPositive' (SObI :! SO :! SO :! SI :! SI) (FPrefixO :? O)
--    Just (FObO :? I :? I :? I :? I)
predFPositive' :: SPositive n -> FPositive n -> Maybe (FPositive n)
predFPositive' = loop id where
  loop :: (FPositive n -> r) -> SPositive n -> FPositive n -> Maybe r
  loop k = flip \cases
    FObO -> \SObI -> Nothing
    FPrefixO -> \(sn :! SI) -> Just $ k $ maxBoundFPositive' sn :? I 
    (fn :? I) -> \_ -> Just $ k (fn :? O) 
    (fn :? O) -> \(sn :! _) -> loop (k . (:? I)) sn fn

-- | A non-partial alternative to 'toEnum'
--
--    >>> toFPositive @(ObI :. O :. O :. O) 5
--    Just (FObO :? I :? O :? I)
--    >>> toFPositive @(ObI :. O :. O :. O) 0
--    Just (FObO :? O :? O :? O)
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
--    Just (FObO :? I :? O :? I)
--    >>> positiveToFPositive @(ObI :. I :. O) (ObI :. O :. I)
--    Just (FPrefixO :? I)
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
--    Just (FObO :? I :? O :? I)
--    >>> positiveToFPositive' (SObI :! SI :! SO) (ObI :. O :. I)
--    Just (FPrefixO :? I)
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
    (sn :! SI) (m :. O) -> loop (Just $ lt FPrefixO) (lt . (:? O)) sn m
    (sn :! SI) (m :. I) -> loop eq (lt . (:? I)) sn m

-- | Alternative to 'fromEnum', where the value of @n@ is given explicitly.
--
--    >>> fromFPositive' (SObI :! SO :! SO :! SO) (FObO :? I :? O :? I)
--    5
--    >>> fromFPositive' (SObI :! SI :! SO) (FPrefixO :? I)
--    5
fromFPositive' :: SPositive n -> FPositive n -> Int
fromFPositive' = loop 1 0 where
  loop :: Int -> Int -> SPositive n -> FPositive n -> Int
  loop !p !t = \cases
    SObI FObO -> t
    (sn :! _) FPrefixO -> fromSPositive sn * 2 * p + t
    (sn :! _) (fn :? b) -> loop (2*p) (t + p * fromEnum b) sn fn

-- * Containers
-- $
-- Using indexed types to track how many elements are in a container

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

instance Foldable1 (Opt 'I) where
  foldMap1 f (Some a) = f a

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

-- | A variant of 'GHC.Tuple.(,)' where both elements have the same type.
type Pair :: Type -> Type
data Pair a = a :* a
  deriving (Functor, Foldable, Traversable, Eq, Show)

infix 8 :*

instance Foldable1 Pair where
  foldMap1 f (a0 :* a1) = f a0 <> f a1

instance Applicative Pair where
  (f0 :* f1) <*> (a0 :* a1) = f0 a0 :* f1 a1
  pure a = a :* a

-- ** @Tree@
-- $

-- | A pear tree is a linked list of balanced binary trees strictly increasing 
-- in order of size.
--
-- The spine of the data structure is defined by the ':\' constructor, which 
-- forks off larger and larger balanced binary trees until it reaches the 
-- 'Canopy', which holds the largest.
--
-- This data structure is heavily influenced by [Edward Z. Yang's bootstrapped 
-- binary tree](http://blog.ezyang.com/2012/08/statically-checked-perfect-binary-trees/#nested-data-types).
--
-- The simplest pear tree holds one element:
--
-- @
-- Canopy a
--
--   ╽
--   a 
-- @
--
-- Without forking off any smaller trees, extending the spine to increase the 
-- canopy depth doubles the number of elements in the canopy.
--
-- @
-- Canopy (a :* b) :\\ None
--
--     :
--     ╽
--     :*
--   a    b 
--
-- Canopy ((a :* b) :* (c :* d)) :\\ None :\\ None
--
--          :
--          :
--          ╽
--          :*
--     :*        :*
--   a    b    c    d 
--
-- Canopy (((a :* b) :* (c :* d)) :* ((e :* f) :* (g :* h))) :\\ None :\\ None :\\ None
--
--                    :
--                    :
--                    :
--                    ╽
--                    :*
--          :*                  :*         
--     :*        :*        :*        :*    
--   a    b    c    d    e    f    g    h   
-- @
--
-- The smaller trees come into play when trying to store a non-power of 2 number 
-- of elements.
--
-- @
-- Canopy (((a :* b) :* (c :* d)) :* ((e :* f) :* (g :* h))) :\\ None :\\ Some (i :* j) :\\ Some k
--
--                    :\\_____________________________.
--                    :\\______________________.      k
--                    :                       :*
--                    ╽                     i    j
--                    :*
--          :*                  :*         
--     :*        :*        :*        :*     
--   a    b    c    d    e    f    g    h   
-- @
--
type Tree :: Positive -> Type -> Type
data Tree n a where
  Canopy :: a -> Tree ObI a
  (:\) :: Tree n (Pair a) -> Opt b a -> Tree (n :. b) a

infixl 4 :\

deriving instance Functor (Tree n)
deriving instance Foldable (Tree n)
deriving instance Traversable (Tree n)
deriving instance Eq a => Eq (Tree n a)

instance Foldable1 (Tree n) where
  foldMap1 f = \case
    Canopy a -> f a
    taa :\ None -> foldMap1 (foldMap1 f) taa
    taa :\ Some a -> foldMap1 (foldMap1 f) taa <> f a

instance Show a => Show (Tree n a) where
  showsPrec p = \case
    taa :\ oa -> showParen (p >= 4) do
      shows taa . showString " :\\ " . shows oa
    Canopy a -> showParen (p >= 10) do
      showString "Canopy " . showsPrec 10 a

instance KnownPositive n => Applicative (Tree n) where
  liftA2 = liftT2
  pure a = generate (const a)

-- | Get the element at a particular offset in a tree.
--
--    >>> let t = Canopy ((('a' :* 'b') :* ('c' :* 'd')) :* (('e' :* 'f') :* ('g' :* 'h'))) :\ None :\ Some ('i' :* 'j') :\ None
--    >>> lookup (FObO :? O :? O :? O) t
--    'a'
--    >>> lookup (FObO :? I :? O :? I) t
--    'f'
--    >>> lookup (FPrefixO :? O) t
--    'i'
--
-- To modify the element at an offset, see 'atTree'.
lookup :: FPositive n -> Tree n a -> a
lookup fn = getConst . atTree fn Const where

-- | Alternative to 'liftA2' without the 'KnownPositive' constraint.
--
--    >>> let t = Canopy ((0 :* 1) :* (2 :* 3)) :\ Some (4 :* 5) :\ Some 6
--    >>> liftT2 (+) t (fmap (10*) t)
--    Canopy ((0 :* 11) :* (22 :* 33)) :\ Some (44 :* 55) :\ Some 66
liftT2 :: (a -> b -> c) -> Tree m a -> Tree m b -> Tree m c
liftT2 f = \cases
  (taa :\ oa) (tbb :\ ob) -> liftT2 (liftA2 f) taa tbb :\ liftO2 f oa ob
  (Canopy a) (Canopy b) -> Canopy (f a b)

-- | Create a 'Tree' with a known number of elements. See also 'pure'.
--
--    >>> generate @(ObI :. I :. I) fromEnum
--    Canopy ((0 :* 1) :* (2 :* 3)) :\ Some (4 :* 5) :\ Some 6
generate :: KnownPositive n => (FPositive n -> a) -> Tree n a
generate = flip generate' knownPositive

-- | Alternative to 'generate' where the value of @n@ is given explicitly.
--
--    >>> generate' fromEnum (SObI :! SI :! SI)
--    Canopy ((0 :* 1) :* (2 :* 3)) :\ Some (4 :* 5) :\ Some 6
generate' :: (FPositive n -> a) -> SPositive n -> Tree n a
generate' f = \case
  SObI -> Canopy (f FObO)
  sn :! sb -> generate' (\fn -> f (fn :? O) :* f (fn :? I)) sn :\ case sb of
    SO -> None
    SI -> Some (f FPrefixO)

-- | Count the number of elements in a 'Tree' as a 'SPositive'.
--
-- In some ways this is the inverse of 'generate''.
--
--    >>> treeSize $ Canopy 'a'
--    SObI
--    >>> treeSize $ Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('e' :* 'f') :\ None
--    SObI :! SI :! SO
treeSize :: Tree n a -> SPositive n
treeSize = \case
  Canopy _ -> SObI
  taa :\ oa -> treeSize taa :! optSize oa

-- | Create a tree from a nonempty list and pass it to a continuation
--
--    >>> fromNonEmpty print $ 'a' :| "bcdefg"
--    Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('e' :* 'f') :\ Some 'g'
--    >>> fromNonEmpty print $ 'a' :| "bcdefgh"
--    Canopy ((('a' :* 'b') :* ('c' :* 'd')) :* (('e' :* 'f') :* ('g' :* 'h'))) :\ None :\ None :\ None
fromNonEmpty :: forall r a. (forall n. Tree n a -> r) -> NonEmpty a -> r
-- the continuation trick is necessary because @NonEmpty a -> Tree n a@ would 
-- mean that the caller gets to specify the type @n@ at compile time, while we 
-- actually want to be able to have the input determine the type at runtime.
fromNonEmpty f = \(a :| as) -> loop (Canopy a) as where
  loop :: Tree n a -> [a] -> r
  loop t = \case
    [] -> f t
    a : as -> loop (push t a) as

-- *** 'push' and 'pop': adding and removing a single element
-- $

-- | type-level 'succ' for positive numbers
type Succ :: Positive -> Positive
-- reusing 'Add' makes sense for compatibility, though I do wish I'd been able to 
-- come up with an injective definition of Succ
type Succ n = Add n ObI

-- | Add a new element to the end of a tree
--
--    >>> push (Canopy 'a') 'b'
--    Canopy ('a' :* 'b') :\ None
--    >>> push (Canopy ('a' :* 'b') :\ None) 'c'
--    Canopy ('a' :* 'b') :\ Some 'c'
--    >>> push (Canopy ('a' :* 'b') :\ Some 'c') 'd'
--    Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None
push :: Tree n a -> a -> Tree (Succ n) a
push t = fuse t . Canopy

-- | type level 'pred' for positive numbers
type Pred :: Positive -> Positive
-- this is necessarily partial, as ObI has no predecessor.
--
-- 'push' and 'pop' are good motivations for supporting, at least in part
-- nonnegative numbers, as `Pred :: Positive -> Nonnegative` would be total.
type family Pred n where
  Pred (ObI :. O) = ObI
  Pred (n :. I) = n :. O
  Pred (bs :. b :. O) = Pred (bs :. b) :. I

-- | Remove the last element from a tree with multiple elements
--
--    >>> pop (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None)
--    (Canopy ('a' :* 'b') :\ Some 'c','d')
--    >>> pop (Canopy ('a' :* 'b') :\ Some 'c')
--    (Canopy ('a' :* 'b') :\ None,'c')
--    >>> pop (Canopy ('a' :* 'b') :\ None)
--    (Canopy 'a','b')
--
-- Note that this cannot be called on a @'Tree' ObI a@
pop :: n ~ (bs :. b) => Tree n a -> (Tree (Pred n) a, a)
pop = \case
  Canopy (a0 :* a1) :\ None -> (Canopy a0, a1)
  taa :\ Some a1 -> (taa :\ None, a1)
  taa@(_ :\ _) :\ None -> case pop taa of
    (taa', a0 :* a1) -> (taa' :\ Some a0, a1)

-- | Offsets for tree elements after a 'push' according to their positions from before the 'push'
type FSucc :: Positive -> Type
type FSucc n = FAdd n ObI

-- | Transform between offsets before a 'push' and after (or from after a 'pop' 
-- and before).
--
--    >>> let rx = offsetSucc (SObI :! SI :! SI)
--
-- Any valid offset before the push can be converted into a post-push outset.
--
--    >>> forwards rx $ FL (FObO :? O :? O)
--    FObO :? O :? O :? O
--    >>> forwards rx $ FL (FPrefixO :? I)
--    FObO :? I :? O :? I
--    >>> forwards rx $ FL FPrefixO
--    FObO :? I :? I :? O
--
-- And vice versa
--
--    >>> backwards rx $ FObO :? O :? O :? O
--    FL (FObO :? O :? O)
--    >>> backwards rx $ FObO :? I :? I :? O
--    FL FPrefixO
--
-- @'FR' 'FObO'@ indicates that the new offset corresponds to the pushed 
-- element.
--
--    >>> backwards rx $ FObO :? I :? I :? I
--    FR FObO
--
-- See 'offsetAdd' for more details.
offsetSucc :: SPositive n -> (FSucc n <-> FPositive (Succ n))
offsetSucc sn = offsetAdd sn SObI

-- *** 'fuse' and 'fizz': combining two lists or dividing one in two
-- $ 

-- | type-level (+) for positive numbers
type Add :: Positive -> Positive -> Positive
type Add i j = AddC i j O

-- | Combine two trees into one in O(log n) time.
--
--      >>> fuse (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None) (Canopy ('E' :* 'F') :\ Some 'G')
--      Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('E' :* 'F') :\ Some 'G'
--      >>> fuse (Canopy ('E' :* 'F') :\ Some 'G') (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None)
--      Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('E' :* 'F') :\ Some 'G'
--      >>> fuse (Canopy ('a' :* 'b') :\ Some 'c') (Canopy ('D' :* 'E') :\ Some 'F')
--      Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ None
--
-- Note that @'fuse' t0 t1@ does not place all the elements of @t0@ before all the elements of @t1@. That is
-- @'Data.Foldable1.fold1' ('fuse' t0 t1) /= 'Data.Foldable1.fold1' t0 <> 
-- 'Data.Foldable1.fold1' t1@.  In general 'fuse' is not associative.
--
-- Though this means that the element at offset @i@ of some tree @t0@ is not at offset @i@ in @'fuse' t0 t1@,
-- its new offset is not random and can be computed exactly using @'offsetAdd' (treeSize t0) (treeSize t1)@.
--
-- 'fuse' has an inverse operation, 'fizz'.
fuse :: Tree n a -> Tree m a -> Tree (Add n m) a
fuse t0 t1 = fuseC t0 t1 None

-- | Split one tree into two in O(log n) time.
--
--      >>> let s3 = SObI :! SI
--      >>> let s4 = SObI :! SO :! SO
--      >>> fizz s4 s3 (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('E' :* 'F') :\ Some 'G')
--      (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None,Canopy ('E' :* 'F') :\ Some 'G')
--      >>> fizz s3 s4 (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('E' :* 'F') :\ Some 'G')
--      (Canopy ('E' :* 'F') :\ Some 'G',Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None)
--      >>> fizz s3 s3 (Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ None)
--      (Canopy ('a' :* 'b') :\ Some 'c',Canopy ('D' :* 'E') :\ Some 'F')
-- 
-- The name \"fizz\" is short for \"fission\".
--
-- Note that in @'fizz' t = (t0, t1)@ not all elements of @t0@ had lower 
-- offsets in @t@ than all elements of @t1@.  Though this means that the 
-- element at offset @i@ of @t@ is not at offset @i@ of @t0@ or @i - |t0|@ of 
-- @t1@, its new location is not random and can be computed exactly using 
-- @'offsetAdd' (treeSize t0) (treeSize t1)@.
--
-- 'fizz' has an inverse operation, 'fuse'.
fizz :: SPositive n -> SPositive m -> Tree (Add n m) a -> (Tree n a, Tree m a)
fizz sn sm t = (tn, tm)
  where (tn, tm, None) = fizzC sn sm SO t

-- | A variant of @'Either' ('FPositive' n) ('FPositive' m) for representing 
-- offsets of elements from two trees of the specified sizes.
--
-- These can be converted to offsets in a single combined tree using 'offsetAdd'.
--
-- 'FAdd' has two constructors
--
-- @
-- 'FL' :: 'FPositive' n -> 'FAdd' n m
-- 'FR' :: 'FPositive' m -> 'FAdd' n m
-- @
type FAdd :: Positive -> Positive -> Type
type FAdd n m = FAddC n m O

-- | Bijections for converting offsets in two unfused trees (e.g. the input 
-- to 'fuse' or the output of 'fizz') into offsets in a single combined tree 
-- (e.g. the output of 'fuse' or the inputs of 'fizz').
--
--      >>> let t0 = Canopy ('a' :* 'b') :\ Some 'c'
--      >>> let t1 = Canopy ('D' :* 'E') :\ Some 'F'
--      >>> let t = fuse t0 t1
--      >>> let bij = offsetAdd (treeSize t0) (treeSize t1)
--      >>> t
--      Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ None
--      >>> let off = FObO :? O
--      >>> lookup off t0
--      'a'
--      >>> lookup (forwards bij $ FL off) t
--      'a'
--      >>> lookup off t1
--      'D'
--      >>> lookup (forwards bij $ FR off) t
--      'D'
offsetAdd :: SPositive n -> SPositive m -> (FAdd n m <-> FPositive (Add n m))
offsetAdd sn sm = offsetAddC sn sm SO

-- *** 'fuseC' and 'fizzC': implementing addition with carry
-- $
-- 'fuse', 'fizz', and 'offsetAdd' above work by implementing the binary 
-- addition algorithm, which in the recursive case, requires a carry bit.
-- This additional logic is implemented for them by 'fuseC', 'fizzC', and 
-- 'offsetAddC', respectively.

-- | Combine two trees and an optional extra element into one tree in O(log n) time.
-- This is a more general case of 'fuse'.
--
--      >>> fuseC (Canopy 'a') (Canopy ('c' :* 'd') :\ None) None
--      Canopy ('c' :* 'd') :\ Some 'a'
--      >>> fuseC (Canopy 'a') (Canopy ('c' :* 'd') :\ None) (Some 'e')
--      Canopy (('c' :* 'd') :* ('a' :* 'e')) :\ None :\ None
--
-- Note that @'fuseC' t0 t1 o@ does not place all the elements of @t0@ before 
-- all the elements of @t1@. In general 'fuseC' is not associative.
--
-- 'fuseC' has an inverse operation, 'fizzC'.  Updated offsets can be computed 
-- using 'offsetAddC'.
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

-- | Split one tree into two and a possible extra eleemnt in O(log n) time. 
-- This is a more general case of 'fizzC'.
--
--      >>> let s1 = SObI
--      >>> let s2 = SObI :! SO
--      >>> fizzC s1 s2 SO (Canopy ('c' :* 'd') :\ Some 'a')
--      (Canopy 'a',Canopy ('c' :* 'd') :\ None,None)
--      >>> fizzC s1 s2 SI (Canopy (('c' :* 'd') :* ('a' :* 'e')) :\ None :\ None)
--      (Canopy 'a',Canopy ('c' :* 'd') :\ None,Some 'e')
--
-- Note that in @'fizzC' s0 s1 t = (t0,t1,o)@, not all the elements of @t0@ 
-- occur before all the elements of @t1@ in @t@.
--
-- The name \"fizz\" is short for \"fission\".
--
-- 'fizzC' has an inverse operation, 'fizzC'.  Updated offsets can be computed 
-- using 'offsetAddC'.
fizzC :: SPositive n -> SPositive m -> SBit b -> Tree (AddC n m b) a -> (Tree n a, Tree m a, Opt b a)
fizzC = \cases
  SObI SObI _ (Canopy (a0 :* a1) :\ oa) ->
    (Canopy a0, Canopy a1, oa)
  (_ :! SO) SObI SO (taa :\ Some a1) ->
    (taa :\ None, Canopy a1, None)
  (sn :! SO) SObI SI (taa :\ None) ->
    let ~(taa0, Canopy (a1 :* a2)) = fizz sn SObI taa
    in (taa0 :\ None, Canopy a1, Some a2)
  (sn :! SI) SObI _ (taa :\ oa) ->
    let ~(taa0, Canopy (a0 :* a1)) = fizz sn SObI taa
    in (taa0 :\ Some a0, Canopy a1, oa)
  SObI (_ :! SO) SO (taa :\ Some a0) ->
    (Canopy a0, taa :\ None, None)
  SObI (sm :! SO) SI (taa :\ None) ->
    let ~(taa1, Canopy (a0 :* a2)) = fizz sm SObI taa
    in (Canopy a0,  taa1 :\ None, Some a2)
  SObI (sm :! SI) _ (taa :\ oa) ->
    let ~(taa1, Canopy (a0 :* a1)) = fizz sm SObI taa
    in (Canopy a0, taa1 :\ Some a1, oa)
  (sn :! SO) (sm :! SO) _ (taa :\ oa) ->
    let ~(taa0, taa1) = fizz sn sm taa
    in (taa0 :\ None, taa1 :\ None, oa)
  (sn :! SO) (sm :! SI) SO (taa :\ oa) ->
    let ~(taa0, taa1) = fizz sn sm taa
    in (taa0 :\ None, taa1 :\ oa, None)
  (sn :! SO) (sm :! SI) SI (taa :\ None) ->
    let ~(taa0, taa1, Some (a1 :* a2)) = fizzC sn sm SI taa
    in (taa0 :\ None, taa1 :\ Some a1, Some a2)
  (sn :! SI) (sm :! SO) SO (taa :\ oa) ->
    let ~(taa0, taa1) = fizz sn sm taa
    in (taa0 :\ oa, taa1 :\ None, None)
  (sn :! SI) (sm :! SO) SI (taa :\ None) ->
    let ~(taa0, taa1, Some (a0 :* a2)) = fizzC sn sm SI taa
    in (taa0 :\ Some a0, taa1 :\ None, Some a2)
  (sn :! SI) (sm :! SI) _ (taa :\ oa) ->
    let ~(taa0, taa1, Some (a0 :* a1)) = fizzC sn sm SI taa
    in (taa0 :\ Some a0, taa1 :\ Some a1, oa)

-- | type-level addition with a carry bit for positive numbers
type AddC :: Positive -> Positive -> Bit -> Positive
-- logically given any three of i,j,k, and b where AddC i j b = k, the fourth 
-- is uniquely determined, but I don't know how to convince GHC of that, so 
-- I'll have to settle for a normal typeclass.
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

-- | A sum type for representing offsets of elements from two trees and an opt of the 
-- specified sizes.
--
-- These can be converted to offsets in a single combined tree using 'offsetAddC'.
type FAddC :: Positive -> Positive -> Bit -> Type
data FAddC n m b where
  FL :: FPositive n -> FAddC n m b
  FR :: FPositive m -> FAddC n m b
  FC :: FAddC n m I -- if we had an FBit type, here's the only place we'd actually use it

deriving instance Eq (FAddC n m b)
deriving instance Show (FAddC n m b)

-- | Bijections for converting offsets in two unfused trees and an opt into 
-- offsets in a single combined tree.
--
--      >>> let t0 = Canopy ('a' :* 'b') :\ Some 'c'
--      >>> let t1 = Canopy ('D' :* 'E') :\ Some 'F'
--      >>> let t = fuseC t0 t1 (Some '-')
--      >>> let bij = offsetAddC (treeSize t0) (treeSize t1) SI
--      >>> t
--      Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ Some '-'
--      >>> let off = FObO :? O
--      >>> lookup off t0
--      'a'
--      >>> lookup (forwards bij $ FL off) t
--      'a'
--      >>> lookup off t1
--      'D'
--      >>> lookup (forwards bij $ FR off) t
--      'D'
--      >>> lookup (forwards bij $ FC) t
--      '-'
offsetAddC :: SPositive n -> SPositive m -> SBit b -> (FAddC n m b <-> FPositive (AddC n m b))
-- we could have combined this with 'fuseC' and/or 'fizzC' to avoid doing the 
-- same logic a third time, but that would make the logic very difficult to follow.
offsetAddC = \cases
  SObI SObI _ -> Bijection
    { forwards = \case
        FL FObO -> FObO :? O
        FR FObO -> FObO :? I
        FC -> FPrefixO
    , backwards = \case
        FObO :? O -> FL FObO
        FObO :? I -> FR FObO
        FPrefixO -> FC
    }
  (_ :! SO) SObI SO -> Bijection
    { forwards = \case
        FL (fn :? b) -> fn :? b
        FR FObO -> FPrefixO
    , backwards = \case
        fn :? b -> FL (fn :? b)
        FPrefixO -> FR FObO
    }
  (sn :! SO) SObI SI -> let bij = offsetSucc sn in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FR FObO -> forwards bij (FR FObO) :? O
        FC -> forwards bij (FR FObO) :? I
    , backwards = \(fx :? b) -> case (backwards bij fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR FObO, O) -> FR FObO
        (FR FObO, I) -> FC
    }
  (sn :! SI) SObI _ -> let bij = offsetSucc sn in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FL FPrefixO -> forwards bij (FR FObO) :? O
        FR FObO -> forwards bij (FR FObO) :? I
        FC -> FPrefixO
    , backwards = \case
        fx :? b -> case (backwards bij fx, b) of
          (FL fn, b) -> FL (fn :? b)
          (FR FObO, O) -> FL FPrefixO
          (FR FObO, I) -> FR FObO
        FPrefixO -> FC
    }
  SObI (_ :! SO) SO -> Bijection
    { forwards = \case
        FL FObO -> FPrefixO
        FR (fn :? b) -> fn :? b
    , backwards = \case
        FPrefixO -> FL FObO
        fn :? b -> FR (fn :? b)
    }
  SObI (sm :! SO) SI -> let bij = offsetSucc sm in Bijection
    { forwards = \case
        FL FObO -> forwards bij (FR FObO) :? O
        FR (fn :? b) -> forwards bij (FL fn) :? b
        FC -> forwards bij (FR FObO) :? I
    , backwards = \(fx :? b) -> case (backwards bij fx, b) of
        (FR FObO, O) -> FL FObO
        (FL fn, b) -> FR (fn :? b)
        (FR FObO, I) -> FC
    }
  SObI (sm :! SI) _ -> let bij = offsetSucc sm in Bijection
    { forwards = \case
        FL FObO -> forwards bij (FR FObO) :? O
        FR (fm :? b) -> forwards bij (FL fm) :? b
        FR FPrefixO -> forwards bij (FR FObO) :? O
        FC -> FPrefixO
    , backwards = \case
        fx :? b -> case (backwards bij fx, b) of
          (FL fn, b) -> FR (fn :? b)
          (FR FObO, O) -> FR FPrefixO
          (FR FObO, I) -> FL FObO
        FPrefixO -> FC
    }
  (sn :! SO) (sm :! SO) _ -> let bij = offsetAdd sn sm in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FR (fm :? b) -> forwards bij (FR fm) :? b
        FC -> FPrefixO
    , backwards = \case
        fx :? b -> case backwards bij fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FPrefixO -> FC
    }
  (sn :! SO) (sm :! SI) SO -> let bij = offsetAdd sn sm in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FR (fm :? b) -> forwards bij (FR fm) :? b
        FR FPrefixO -> FPrefixO
    , backwards = \case
        fx :? b -> case backwards bij fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FPrefixO -> FR FPrefixO
    }
  (sn :! SO) (sm :! SI) SI -> let bij = offsetAddC sn sm SI in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FR (fm :? b) -> forwards bij (FR fm) :? b
        FR FPrefixO -> forwards bij FC :? O
        FC -> forwards bij FC :? I
    , backwards = \(fx :? b) -> case (backwards bij fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR fm, b) -> FR (fm :? b)
        (FC, O) -> FR FPrefixO
        (FC, I) -> FC
    }
  (sn :! SI) (sm :! SO) SO -> let bij = offsetAdd sn sm in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FL FPrefixO -> FPrefixO
        FR (fm :? b) -> forwards bij (FR fm) :? b
    , backwards = \case
        fx :? b -> case backwards bij fx of
          FL fn -> FL (fn :? b)
          FR fm -> FR (fm :? b)
        FPrefixO -> FL FPrefixO
    }
  (sn :! SI) (sm :! SO) SI -> let bij = offsetAddC sn sm SI in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FL FPrefixO -> forwards bij FC :? O
        FR (fm :? b) -> forwards bij (FR fm) :? b
        FC -> forwards bij FC :? I
    , backwards = \(fx :? b) -> case (backwards bij fx, b) of
        (FL fn, b) -> FL (fn :? b)
        (FR fm, b) -> FR (fm :? b)
        (FC, O) -> FL FPrefixO
        (FC, I) -> FC
    }
  (sn :! SI) (sm :! SI) _ -> let bij = offsetAddC sn sm SI in Bijection
    { forwards = \case
        FL (fn :? b) -> forwards bij (FL fn) :? b
        FL FPrefixO -> forwards bij FC :? O
        FR (fm :? b) -> forwards bij (FR fm) :? b
        FR FPrefixO -> forwards bij FC :? I
        FC -> FPrefixO
    , backwards = \case
        fx :? b -> case (backwards bij fx, b) of
          (FL fn, b) -> FL (fn :? b)
          (FC, O) -> FL FPrefixO
          (FR fm, b) -> FR (fm :? b)
          (FC, I) -> FR FPrefixO
        FPrefixO -> FC
    }

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
-- offset.
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

-- | A lens for accessing an arbitrary element of a 'Tree', using 'FPositive' as an offset.
--
--    >>> let t = Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ Some 'e'
--    >>> t & view (atTree (FObO :? O :? O))
--    'a'
--    >>> t & view (atTree (FObO :? I :? O))
--    'c'
--    >>> t & view (atTree FPrefixO)
--    'e'
--    >>> t & set (atTree (FObO :? O :? I)) 'B'
--    Canopy (('a' :* 'B') :* ('c' :* 'd')) :\ None :\ Some 'e'
atTree :: FPositive n -> Lens' (Tree n a) a
atTree = \case
  FObO -> _Canopy
  FPrefixO -> _Branch
  fn :? b -> _Up . atTree fn . atPair b

-- * Misc
-- $

-- | A bijection is a function with an inverse.
--
-- Proper values obey the following law:
--
-- @
-- forwards bij . backwards bij = id :: b -> b
-- backwards bij . forwards bij = id :: a -> a
-- @
type (<->) :: Type -> Type -> Type
data a <-> b = Bijection { forwards :: a -> b, backwards :: b -> a }

instance Category (<->) where
  id = Bijection id id
  Bijection f0 b0 . Bijection f1 b1 = Bijection (f0 . f1) (b1 . b0)

