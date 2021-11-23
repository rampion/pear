{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Data.Natural.Binary.Type 
  ( Bit(..)
  , Binary(..)
  , pattern Zero, isZero
  , pattern Push, pop
  , toCanonical, isCanonical
  , safePred, safeMinus, safeQuotRem
  , foldhi, foldlo
  , toBinary, fromBinary
  , pattern (:+:), d
  ) where

import qualified Data.Natural.Binary.Two as Two
import Data.Natural.Binary.Two (Two((:*)))
import Data.Functor ((<&>))
import GHC.Generics

import Data.Bits

import Data.Function (fix)

pattern (:+:) :: Num a => a -> a -> a
pattern (:+:) x y <- (fix . d -> (x,y))

d :: Num a => a -> (a,a) -> (a,a)
d n ~(x,y) = (n -y, n - x)


type Bit :: *
data Bit = O | I deriving (Eq, Ord, Show, Enum, Generic, Bounded)

instance Bits Bit where
  I .&. I = I
  _ .&. _ = O
  O .|. O = O
  _ .|. _ = I
  I `xor` b = complement b
  O `xor` b = b
  complement O = I
  complement I = O
  shift b 0 = b
  shift _ _ = O
  rotate = const
  bitSize = const 1
  bitSizeMaybe = const (Just 1)
  isSigned = const False
  testBit I 0 = True
  testBit _ _ = False
  bit b
    | even b = O
    | otherwise = I
  popCount O = 0
  popCount I = 1

infixl 4 :.

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

instance Show Binary where
  showsPrec p = showParen (p >= 4) . showBinary where
    showBinary Ob = showString "Ob"
    showBinary (bs :. b) = showBinary bs . showString " :. " . shows b

instance Ord Binary where
  compare = compareCarry EQ where

    compareCarry :: Ordering -> Binary -> Binary -> Ordering
    compareCarry c (as :. a) (bs :. b) = compareCarry (compare a b <> c) as bs
    compareCarry c Zero Zero = c
    compareCarry _ (_ :. _) Ob = GT
    compareCarry _ Ob (_ :. _) = LT

{-# COMPLETE Zero, (:.)  #-}
pattern Zero :: Binary
pattern Zero <- (isZero -> True)
  where Zero = Ob

isZero :: Binary -> Bool
isZero (bs :. O) = isZero bs
isZero (_ :. I)= False
isZero Ob = True

{-# COMPLETE Push #-}
pattern Push :: Binary -> Bit -> Binary
pattern Push bs b <- (pop -> (bs, b))
  where Push bs I = bs :. I
        Push Ob _ = Ob
        Push bs _ = bs :. O

pop :: Binary -> (Binary, Bit)
pop Ob = (Ob, O)
pop (bs :. b) = (bs, b)

isCanonical :: Binary -> Bool
isCanonical (Ob :. O) = False
isCanonical (bs :. _) = isCanonical bs
isCanonical Ob = True

toCanonical :: Binary -> Binary
toCanonical Ob = Ob
toCanonical (bs :. b) = toCanonical bs `Push` b

instance Eq Binary where
  a == b = compare a b == EQ

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

safePred :: Binary -> Maybe Binary
safePred (bs :. O) = safePred bs <&> (:. I)
safePred (Ob :. I) = Just Ob
safePred (bs :. I) = Just (bs :. O)
safePred Ob = Nothing

-- non-strict fold starting with high bits
foldhi :: a -> (a -> Bit -> a) -> Binary -> a
foldhi a f (bs :. b) = f (foldhi a f bs) b
foldhi a _ Ob = a

-- strict fold starting with low bits
foldlo :: a -> (Bit -> a -> a) -> Binary -> a
foldlo !a f (bs :. b) = foldlo (f b a) f bs
foldlo a _ Ob = a

fromBinary :: Integral a => Binary -> a
fromBinary = Two.snd . foldlo (1 :* 0) \b (p :* t) -> 
  (p * 2) :* (t + p * case b of { O -> 0 ; I -> 1 })

toBinary :: Integral a => a -> Binary
toBinary 0 = Ob
toBinary n = case quotRem n 2 of
  (q, 0) -> toBinary q :. O
  (q, ~1) -> toBinary q :. I
    
xor3 :: Bit -> Bit -> Bit -> Bit
xor3 a b c = a `xor` b `xor` c

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
