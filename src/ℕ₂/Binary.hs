{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module ℕ₂.Binary
  ( Bit(..)
  , Binary(..)
  , showBits
  , pattern Zero, isZero
  , pattern Push, pop
  , toCanonical, isCanonical
  , safePred, safeMinus, safeQuotRem
  , foldhi, foldlo
  , toBinary, fromBinary
  ) where

import Numeric (showIntAtBase)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Data.Bits (Bits(..))
import Data.Function (fix)

import ℕ₂.Bit (Bit(..))
import ℕ₂.Two (Two((:*)))
import qualified ℕ₂.Two as Two

-- $setup
-- >>> :set -XBlockArguments -XLambdaCase -XBinaryLiterals
-- >>> import Data.Function ((&))

infixl 4 :.

-- | A binary encoding of the natural numbers
--
-- @
--    0 ~ Ob 
--    1 ~ Ob :. I
--    2 ~ Ob :. I :. O
--    3 ~ Ob :. I :. I
--    …
-- @
--
type Binary :: *
data Binary = Ob | Binary :. Bit
  deriving Generic

-- This representation is simple, and isomorphic to [Bit] but has a slight
-- advantage in that the least significant bit is on the right, rather than the
-- left.
--
-- It's not perfect, as it still admits multiple equivalent values, with
-- differing numbers of leading zeroes (e.g. Ob, Ob:.O, Ob:.O:.O).
--
-- This could be fixed by use of a GADT:
--
--    data IsZero = Zero | NonZero
--    data Parity = Even | Odd
--
--    data Bit (parity :: Parity) where
--      O :: Bit 'Even
--      I :: Bit 'Odd
--
--    data Bits (isZero :: IsZero) (parity :: Parity) where
--      ObO :: Bits 'Zero 'Even
--      ObI :: Bits 'NonZero 'Odd
--      (:.) :: Bits 'NonZero __ -> Bit parity -> Bits 'NonZero parity
--
--    data Binary where
--      Binary :: Bits isZero parity -> Binary
--
--  But this does complicate later type-level work.
--
--  For now, it seems a better tradeoff to have a single canonical representation
--  (no leading zeroes), and not worry about the equivalence of non-canonical
--  representations at the type level.

-- |
-- Represents canonical values as binary literals (@-XBinaryLiterals@), and
-- non-canonical values using ':.' and 'Bit' constructors.
--
--  >>> Ob
--  0b0
--  >>> Ob :. I :. I :. O :. I
--  0b1101
--  >>> Ob :. O :. I :. I :. O :. I
--  Ob :. O :. I :. I :. O :. I
--
instance Show Binary where
  showsPrec p n 
    | isCanonical n = showString "0b" . showIntAtBase 2 digit n
    | otherwise = showParen (p >= 4) (showBits n)
    where
      digit :: Int -> Char
      digit = toEnum . (+ fromEnum '0')

-- |
-- Builds a representation of a 'Binary' using its constructors
--
--  >>> showBits Ob ""
--  "Ob"
--  >>> showBits (Ob :. I :. I :. O :. I) ""
--  "Ob :. I :. I :. O :. I"
--  >>> showBits (Ob :. O :. I :. I :. O :. I) ""
--  "Ob :. O :. I :. I :. O :. I"
--
showBits :: Binary -> ShowS
showBits Ob = showString "Ob"
showBits (bs :. b) = showBits bs . showString " :. " . shows b

-- $Ord
--
-- A custom 'Ord' instance is used to preserve equivalence between
-- canonical and non-canonical representations.
--
-- >>> compare (Ob :. O :. O) (Ob :. I)
-- LT
-- >>> compare Ob (Ob :. O)
-- EQ

instance Ord Binary where
  compare = compareCarry EQ where

    compareCarry :: Ordering -> Binary -> Binary -> Ordering
    compareCarry c (as :. a) (bs :. b) = compareCarry (compare a b <> c) as bs
    compareCarry c Zero Zero = c
    compareCarry _ (_ :. _) Ob = GT
    compareCarry _ Ob (_ :. _) = LT

-- |
-- Pattern matching the canonical zero ('Ob') and non-canonical zero values
-- (@Ob :. O@, @Ob :. O :. O@, …)
pattern Zero :: Binary
pattern Zero <- (isZero -> True)
  where Zero = Ob
{-# COMPLETE Zero, (:.)  #-}


-- |
-- Predicate for canonical and non-canonical zero values
--
-- >>> isZero Ob
-- True
-- >>> isZero do Ob :. O
-- True
-- >>> isZero do Ob :. I
-- False
-- >>> isZero do Ob :. O :. O
-- True
-- >>> isZero do Ob :. O :. I
-- False
isZero :: Binary -> Bool
isZero (bs :. O) = isZero bs
isZero (_ :. I)= False
isZero Ob = True

-- |
-- Pattern splitting a binary number n into a binary number m and a bit b such
-- that n = 2*m + b
--
-- >>> Push Ob O
-- 0b0
-- >>> 0b0 & \case Push Ob O -> ()
-- ()
-- >>> Push Ob I
-- 0b1
-- >>> 0b1 & \case Push Ob I -> ()
-- ()
-- >>> Push 0b101 O
-- 0b1010
-- >>> 0b1010 & \case Push 0b101 O -> ()
-- ()
-- >>> Push 0b101 I
-- 0b1011
-- >>> 0b1011 & \case Push 0b101 I -> ()
-- ()
--
-- It preserves canaonical representations, but does not normalize
-- non-canonical representations.
--
-- >>> Push (Ob :. O) O
-- Ob :. O :. O
-- >>> Push (Ob :. O :. I) O
-- Ob :. O :. I :. O
pattern Push :: Binary -> Bit -> Binary
pattern Push bs b <- (pop -> (bs, b))
  where Push bs I = bs :. I
        Push Ob _ = Ob
        Push bs _ = bs :. O
{-# COMPLETE Push #-}

-- |
-- Split a binary number n into a binary number m and a bit b such
-- that n = 2*m + b
--
-- >>> pop 0
-- (0b0,O)
-- >>> pop 1
-- (0b0,I)
-- >>> pop 2
-- (0b1,O)
pop :: Binary -> (Binary, Bit)
pop Ob = (Ob, O)
pop (bs :. b) = (bs, b)

-- |
-- Predicate for checking if a given binary number uses the canonical encoding
-- (i.e. does not have any leading zeroes).
--
-- >>> isCanonical Ob
-- True
-- >>> isCanonical do Ob :. I :. I :. O :. I
-- True
-- >>> isCanonical do Ob :. O :. I :. I :. O :. I
-- False
isCanonical :: Binary -> Bool
isCanonical (Ob :. O) = False
isCanonical (bs :. _) = isCanonical bs
isCanonical Ob = True

-- |
-- Converts any 'Binary' to its canonical encoding
--
-- >>> toCanonical Ob
-- 0b0
-- >>> toCanonical do Ob :. I :. I :. O :. I
-- 0b1101
-- >>> toCanonical do Ob :. O :. I :. I :. O :. I
-- 0b1101
toCanonical :: Binary -> Binary
toCanonical Ob = Ob
toCanonical (bs :. b) = toCanonical bs `Push` b

instance Eq Binary where
  a == b = compare a b == EQ

-- |
-- convenience method for constructing partial functions from total ones
orThrow :: Maybe a -> String -> a
orThrow Nothing msg = error msg
orThrow (Just a) _ = a

instance Enum Binary where
  succ (bs :. I) = succ bs :. O
  succ (bs :. O) = bs :. I
  succ Ob = Ob :. I
  toEnum = toBinary
  fromEnum = fromBinary
  pred m = safePred m `orThrow` "pred(Binary): tried to take `pred` of first tag in enumeration"
  enumFromThen base inc = iterate (+inc) base

instance Bounded Binary where
  minBound = Ob
  maxBound = fix (:.I)

-- |
-- The predecessor of the input, if one exists
--
-- >>> safePred Ob
-- Nothing
-- >>> safePred do Ob :. I :. I :. O :. I
-- Just 0b1100
-- >>> safePred do Ob :. O :. I :. I :. O :. I
-- Just (Ob :. O :. I :. I :. O :. O)
--
-- Preserves canonical encodings.
safePred :: Binary -> Maybe Binary
safePred (bs :. O) = safePred bs <&> (:. I)
safePred (Ob :. I) = Just Ob
safePred (bs :. I) = Just (bs :. O)
safePred Ob = Nothing

-- |
-- A non-strict fold starting with high bits
--
-- >>> foldhi Ob (:.) 0b10110110
-- 0b10110110
--
-- Since it is non-strict, the output may be defined even on infinite inputs.
--
-- >>> foldhi False (\r b -> b == O || r) Ob
-- False
-- >>> foldhi False (\r b -> b == O || r) do Ob :. I :. O :. I
-- True
-- >>> foldhi False (\r b -> b == O || r) do fix (:. I) :. O :. I
-- True
foldhi :: a -> (a -> Bit -> a) -> Binary -> a
foldhi a f (bs :. b) = f (foldhi a f bs) b
foldhi a _ Ob = a

-- |
-- A strict fold starting with low bits
--
-- >>> foldlo Ob (\b r -> r :. b) 0b10110001
-- 0b10001101
-- >>> foldlo 0 (\case O -> id ; I -> succ) 0b10110001
-- 4
foldlo :: a -> (Bit -> a -> a) -> Binary -> a
foldlo !a f (bs :. b) = foldlo (f b a) f bs
foldlo a _ Ob = a

-- |
-- Convert a 'Binary' into any integral value
--
-- >>> fromBinary 0b10110001 :: Int
-- 177
-- >>> fromBinary 0b10110001 :: Integer
-- 177
-- >>> fromBinary (Ob :. O :. I :. O :. I :. I :. O :. O :. O :. I)
-- 177
fromBinary :: Integral a => Binary -> a
fromBinary = Two.snd . foldlo (1 :* 0) \b (p :* t) -> 
  (p * 2) :* (t + p * case b of { O -> 0 ; I -> 1 })

-- |
-- Convert an integral value into a 'Binary'
--
-- >>> toBinary (177 :: Int)
-- 0b10110001
-- >>> toBinary (177 :: Integer)
-- 0b10110001
toBinary :: Integral a => a -> Binary
toBinary 0 = Ob
toBinary n = case quotRem n 2 of
  (q, 0) -> toBinary q :. O
  (q, ~1) -> toBinary q :. I
    
-- |
-- xor three bits
--
-- >>> xor3 I O I
-- O
-- >>> xor3 I I I
-- I
xor3 :: Bit -> Bit -> Bit -> Bit
xor3 a b c = a `xor` b `xor` c

-- |
-- Subtract the second argument from the first,
-- representing underflow with 'Nothing'.
--
-- >>> safeMinus 0b1011 0b11
-- Just 0b1000
-- >>> safeMinus 0b1011 0b110
-- Just 0b101
-- >>> safeMinus 0b1011 0b1100
-- Nothing
safeMinus :: Binary -> Binary -> Maybe Binary
safeMinus = \ps qs -> safeMinus' ps qs O where

  safeMinus' :: Binary -> Binary -> Bit -> Maybe Binary
  safeMinus' (ps :. p) (qs :. q) !b = safeMinus' ps qs  (borrow p q b) <&> \case
      Ob | r == O -> Ob
      rs -> rs :. r
    where r = xor3 p q b
  safeMinus' Ob qs O | qs == Zero = Just Ob
  safeMinus' Ob _ _   = Nothing
  safeMinus' ps Ob O  = Just ps
  safeMinus' ps Ob I  = safePred ps

  borrow :: Bit -> Bit -> Bit -> Bit
  borrow O = (.|.)
  borrow I = (.&.)

instance Num Binary where
  (+) = \as bs -> plus as bs O where
    plus :: Binary -> Binary -> Bit -> Binary
    plus (as :. a) (bs :. b) !c = plus as bs (carry a b c) :. xor3 a b c
    plus as Ob O = as
    plus Ob bs O = bs
    plus (as :. O) Ob I = as :. I
    plus (as :. I) Ob I = succ as :. O
    plus Ob (bs :. O) I = bs :. I
    plus Ob (bs :. I) I = succ bs :. O
    plus Ob Ob I = Ob :. I

    carry :: Bit -> Bit -> Bit -> Bit
    carry O = (.&.)
    carry I = (.|.)

  x - y = safeMinus x y `orThrow` "arithmetic underflow"

  (*) Ob = const Ob
  (*) m = foldhi Ob \t -> \case
    O -> t :. O
    I -> (t :. O) + m

  abs = id
  signum Zero = Ob
  signum _ = Ob :. I
  fromInteger = toBinary

instance Real Binary where
  toRational = toRational . toInteger

-- |
-- Divide the first argument by the second, returning the
-- quotient and remainder, representing division by zero
-- with 'Nothing'.
--
-- >>> safeQuotRem 0b100 0b101
-- Just (0b0,0b100)
-- >>> safeQuotRem 0b101 0b101
-- Just (0b1,0b0)
-- >>> safeQuotRem 0b10111 0b101
-- Just (0b100,0b11)
-- >>> safeQuotRem 0b10111 0b0
-- Nothing
safeQuotRem :: Binary -> Binary -> Maybe (Binary, Binary)
safeQuotRem _ Zero = Nothing
safeQuotRem m n = Just do loop m where
  loop (m :. b) = 
    let (q, (`Push` b) -> r) = loop m in 
    case safeMinus r n of
      Nothing -> (Push q O, r)
      Just r  -> (Push q I, r)
  loop Ob = (Ob, Ob)

instance Integral Binary where
  quotRem m n = safeQuotRem m n `orThrow` "division by zero"
  toInteger = fromBinary

instance Bits Binary where
  (as :. a) .&. (bs :. b) = Push (as .&. bs) (a .&. b)
  _ .&. _ = Ob

  (as :. a) .|. (bs :. b) = (as .|. bs) :. (a .|. b)
  Ob .|. bs = bs
  as .|. Ob = as

  (as :. a) `xor` (bs :. b) = Push (as `xor` bs) (a `xor` b)
  Ob `xor` bs = bs
  as `xor` Ob = as

  complement _ = error "Data.Natural.Binary.Type.complement: Natural complement undefined"

  shift bs 0 = bs
  shift bs n
    | n >= 0 = iterate (:. O) bs !! n
    | otherwise = iterate (fst . pop) bs !! negate n

  rotate = shift

  bit 0 = Ob :. I
  bit n = bit (n - 1) :. O

  testBit Ob _ = False
  testBit (_ :. b) 0 = testBit b 0
  testBit (bs :. _) n = testBit bs (n - 1)

  isSigned _ = False

  bitSizeMaybe _ = Nothing
  bitSize _ = error "Data.Natural.Binary.Type.bitSize"
  popCount = foldlo 0 (\b n -> popCount b + n)
