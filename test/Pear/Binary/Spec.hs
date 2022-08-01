{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pear.Binary.Spec where

import GHC.Natural
import GHC.Generics

import Data.GenValidity

import Test.Hspec
import Test.Validity
import Test.QuickCheck

import Pear.Binary
import Pear.Bit.Spec ()

instance Validity Binary
instance GenValid Binary

instance Arbitrary Binary where
  arbitrary = genValid

sameBits :: Binary -> Binary -> Bool 
sameBits (as :. a) (bs :. b) = (a == b) && (as `sameBits` bs)
sameBits Ob Ob = True
sameBits _ _ = False

newtype Canonical = Canonical { getCanonical :: Binary }
  deriving stock (Ord, Eq, Generic)
  deriving anyclass GenValid
  deriving newtype (Show, Num)

instance Arbitrary Canonical where
  arbitrary = genValid

instance Validity Canonical where
  validate (Canonical n) = check (isCanonical n) "uses a canonical encoding"

commute :: (Integer -> Integer) -> Binary -> Binary
commute f = fromInteger . f . toInteger

fcommute :: Functor f => (Integer -> f Integer) -> Binary -> f Binary
fcommute f = fmap fromInteger . f . toInteger

commute2 :: (Integer -> Integer -> Integer) -> Binary -> Binary -> Binary
commute2 f (toInteger -> m) (toInteger -> n) = fromInteger do f m n

fcommute2 :: Functor f => (Integer -> Integer -> f Integer) -> Binary -> Binary -> f Binary
fcommute2 f (toInteger -> m) (toInteger -> n) = fromInteger <$> f m n

binarySpec :: Spec
binarySpec = describe "Binary" do
  eqSpec @Binary
  ordSpec @Binary
    
  it "contains an embedding of the naturals" do
    equivalent (id @Natural) (fromBinary . toBinary)

  describe "an embedding in the naturals" do
    it "preserves equivalence" do
      id `equivalent` commute id

    it "preserves canonical representations" do
      property \(Canonical n) ->
        n `sameBits` (fromInteger . toInteger) n

  describe "fromInteger" do
    it "produces a canonical representation" do
      property (isCanonical . fromInteger)

  describe "Enum instance" do
    it "preserves enumeration order for succ" do
      succ `equivalent` commute succ

    it "preserves enumeration order for pred" do
      safePred `equivalent` fcommute \case
        0 -> Nothing
        n -> Just (pred n)

    it "has pred act as an left inverse to succ" do
      id @Binary `equivalent` (pred . succ)

    it "preserves canonical representation for succ" do
      property (isCanonical . succ . getCanonical)

    it "preserves canonical representation for pred" do
      property (isCanonical . pred . getCanonical . getPositive)

  describe "Num instance" do
    it "preserves addition" do
      (+) `equivalent2` commute2 (+)

    it "preserves canonical representation for addition" do
      property \(Canonical m) (Canonical n) ->
        isCanonical (m + n)

    it "preserves multiplication" do
      (*) `equivalent2` commute2 (*)

    it "preserves canonical representation for multiplication" do
      property \(Canonical m) (Canonical n) ->
        isCanonical (m * n)

    it "preserves sign" do
      signum `equivalent` commute signum

    it "preserves subtraction" do
      safeMinus `equivalent2` fcommute2 \m n ->
        if n <= m
          then Just (m - n)
          else Nothing

    it "preserves canonical representation for subtraction" do
      property \(Canonical m) (Canonical n) -> maybe True isCanonical do safeMinus m n 

  describe "Integral instance" do
    it "preserves division with remainder" do
      property \m n -> 
        m `safeQuotRem` n == case n of
          Zero -> Nothing
          _ -> Just do
            let (q,r) = toInteger m `quotRem` toInteger n
             in (fromInteger q, fromInteger r)
      
    it "preserves canonical representation for division with remainder" do
      property \(Canonical m) (Canonical n) -> 
        case safeQuotRem m n of
          Nothing -> True
          Just (q,r) -> isCanonical q && isCanonical r
