{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Binary.Finite
  ( FBinary(..)
  , safeSucc
  , safePred
  , safeToEnum
  ) where

-- import Data.Bits
import Pear.Bit
import Pear.Bit.Singleton
import Pear.Binary hiding (safePred)
import Pear.Finite
import Pear.Function
import Pear.Singleton
import Pear.Singleton.Known
import Pear.Binary.Singleton

import Data.Maybe (fromMaybe)


-- $setup
-- >>> :set -XTypeOperators -XDataKinds -XBinaryLiterals

-- |
-- A finite datatype for 'Binary', such that @FBinary n@ corresponds
-- to the @n@ natural numbers less than @n@.
--
-- For example consider @FBinary 0b1011@; its eleven elements are:
--
-- @
--  0b0000 = (Top :: FBinary 0b1) :! O :! O :! O
--  0b0001 = (Top :: FBinary 0b1) :! O :! O :! I
--  0b0010 = (Top :: FBinary 0b1) :! O :! I :! O
--  0b0011 = (Top :: FBinary 0b1) :! O :! I :! I
--  0b0100 = (Top :: FBinary 0b1) :! I :! O :! O
--  0b0101 = (Top :: FBinary 0b1) :! I :! O :! I
--  0b0110 = (Top :: FBinary 0b1) :! I :! I :! O
--  0b0111 = (Top :: FBinary 0b1) :! I :! I :! I
--  0b1000 = (Top :: FBinary 0b101) :! O
--  0b1001 = (Top :: FBinary 0b101) :! I
--  0b1010 = Top :: FBinary 0b1011
-- @
--
-- In general, 'Top' represents the bit pattern corresponding to the bits above its position
-- in the binary parameter, replacin the @I@ at its position with a @O@.
--
-- That is:
--
-- @
--  t :: Binary
--  t = Ob :. t_m :. t_{m-1} :. ... :. t_{k+1} :.   I :. t_{k-1} :. ... :. t_2 :. t_1 :. t_0
--
--  i :: FBinary t
--  i =                                           Top :! i_{k-1} :! ... :! i_2 :! i_1 :! i_0
--
--  embed i
--    = Ob :. t_m :. t_{m-1} :. ... :. t_{k+1} :.   O :. i_{k-1} :. ... :. i_2 :. i_1 :. i_0
-- @
type FBinary :: Binary -> *
data FBinary cardinality where
  -- NOTE: constructor declaration order makes deriving Ord work
  (:!) :: !(FBinary bs) -> !Bit -> FBinary (bs ':. b)
  Top :: FBinary (bs ':. 'I)
  -- could possibly drop the Known constraint on much of the following using an alternate defn
  -- for Top:
  --    Top :: SBinary bs -> FBinary (bs ':. 'I)

infixl 4 :!

type instance Fin_ Binary = FBinary

instance Finite FBinary where
  embedding = Function { image = toCanonical . image' sing, preimage = preimage' Nothing Just sing } where
    image' :: forall (t :: Binary). Sing t -> Fin t -> Binary
    image' SOb = \case
    image' (ts ::. t) = 
      let img = image' ts
          bs = demote ts
      in case t of
        SO -> \case
          is :! i -> img is :. i
        SI -> \case
          Top -> bs :. O
          is :! i -> img is :. i

    preimage' :: forall r (t :: Binary). Maybe r -> (Fin t -> Maybe r) -> Sing t -> Binary -> Maybe r
    preimage' eq lt (ts ::. t) = \case
      Push bs b -> preimage'
        do case (t, b) of
            (SO, I) -> Nothing
            (SI, O) -> lt Top
            _       -> eq
        do lt . (:! b)
        do ts
        do bs
    preimage' eq _ SOb = \case
      Zero  -> eq
      _     -> Nothing

-- don't derive Show to avoid extra parens
instance Show (FBinary n) where
  showsPrec = \p -> \case
      n@Top -> showFBinary n 
      n -> showParen (p >= 4) do showFBinary n
    where
      showFBinary :: FBinary n -> ShowS
      showFBinary Top = showString "Top"
      showFBinary (is :! i) = showFBinary is . showString " :! " . shows i

deriving instance Eq (FBinary n)
deriving instance Ord (FBinary n)

-- TODO: Generic

instance Known n => Enum (FBinary n) where
  succ = partial . safeSucc
  pred = partial . safePred
  toEnum = partial . safeToEnum
  fromEnum = fromEnum . embed
  enumFrom = go sing where
    go :: SBinary n -> FBinary n -> [FBinary n]
    go (ts ::. t) (is :! i) =
      case i of { O -> id; I -> tail }
      do foldr 
          do \i is -> (i :! O) : (i :! I) : is
          case t of { SO -> []; SI -> [Top] }
          do go ts is
    go (_ ::. SI) Top = [Top]
          

  -- enumFromTo :: a -> a -> [a]
  -- enumFromThen :: a -> a -> [a]
  -- enumFromThenTo :: a -> a -> a -> [a]

-- | non-partial variant of 'toEnum'
--
-- >>> type ObIOII = 'Ob ':. 'I ':. 'O ':. 'I ':. 'I
-- >>> safeToEnum 0b0 :: Maybe (FBinary ObIOII)
-- Just (Top :! O :! O :! O)
-- >>> safeToEnum 0b1010 :: Maybe (FBinary ObIOII)
-- Just Top
-- >>> safeToEnum 0b1011 :: Maybe (FBinary ObIOII)
-- Nothing
safeToEnum :: Known n => Int -> Maybe (FBinary n)
safeToEnum = select . toEnum

-- | non-partial variant of 'succ'
--
-- >>> type ObIOII = 'Ob ':. 'I ':. 'O ':. 'I ':. 'I
-- >>> safeSucc (Top :! O :! O :! O) :: Maybe (FBinary ObIOII)
-- Just (Top :! O :! O :! I)
-- >>> safeSucc (Top :! I :! I :! I) :: Maybe (FBinary ObIOII)
-- Just (Top :! O)
-- >>> safeSucc (Top :! I) :: Maybe (FBinary ObIOII)
-- Just Top
-- >>> safeSucc Top :: Maybe (FBinary ObIOII)
-- Nothing
safeSucc :: Known n => FBinary n -> Maybe (FBinary n)
safeSucc = \n -> go sing n Nothing Just where
  go :: SBinary n -> FBinary n -> r -> (FBinary n -> r) -> r
  go (ts ::.  t) (is :! I) nothing just = go ts is 
    case t of
      SO -> nothing
      SI -> just Top
    do just . (:! O)
  go ( _ ::.  _) (is :! O) _ just = just (is :! I)
  go ( _ ::. SI) Top nothing _ = nothing

-- | non-partial variant of 'succ'
--
-- >>> type ObIOII = 'Ob ':. 'I ':. 'O ':. 'I ':. 'I
-- >>> safePred (Top :! O :! O :! O) :: Maybe (FBinary ObIOII)
-- Nothing
-- >>> safePred (Top :! O :! I :! O) :: Maybe (FBinary ObIOII)
-- Just (Top :! O :! O :! I)
-- >>> safePred (Top :! O) :: Maybe (FBinary ObIOII)
-- Just (Top :! I :! I :! I)
-- >>> safePred Top :: Maybe (FBinary ObIOII)
-- Just (Top :! I)
safePred :: Known n => FBinary n -> Maybe (FBinary n)
safePred = \n -> go sing n id where
  go :: SBinary n -> FBinary n -> (FBinary n -> r) -> Maybe r
  go (ts ::.  _) (is :! O) f = go ts is (f . (:! I))
  go ( _ ::.  _) (is :! I) f = Just do f (is :! O)
  go (ts ::. SI) Top f = maxBound ts (f . (:! I))

  maxBound :: SBinary n -> (FBinary n -> r) -> Maybe r
  maxBound (ts ::. SO) f = maxBound ts (f . (:! I))
  maxBound ( _ ::. SI) f = Just (f Top)
  maxBound SOb _ = Nothing

partial :: Maybe (FBinary n) -> FBinary n
partial = fromMaybe (error "bad argument")
  
instance Known bs => Bounded (FBinary (bs ':. 'I)) where
  minBound = go sing (:! O) Top where
    go :: SBinary n -> (FBinary n -> r) -> r -> r
    go (ts ::. SO) f r = go ts (f . (:! O)) r
    go (ts ::. SI) f _ = go ts (f . (:! O)) (f Top)
    go SOb _ r = r
  maxBound = Top

instance Bounded (FBinary bs) => Bounded (FBinary (bs ':. 'O)) where
  minBound = minBound :! O
  maxBound = maxBound :! I
