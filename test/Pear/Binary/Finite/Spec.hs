{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Pear.Binary.Finite.Spec 
  ( fbinarySpec
  ) where

import Control.Monad (forM_)

import Test.Hspec

import Pear.Binary hiding (safePred)
import Pear.Binary.Finite
import Pear.Finite

type ObIOII = 'Ob ':. 'I ':. 'O ':. 'I ':. 'I

fbinarySpec :: Spec
fbinarySpec = describe "FBinary" do
  describe "Finite instance" do
    let examples :: [(FBinary ObIOII, Binary)]
        examples =
          [ (Top :! O :! O :! O, 0b000)
          , (Top :! O :! O :! I, 0b001)
          , (Top :! O :! I :! O, 0b010)
          , (Top :! O :! I :! I, 0b011)
          , (Top :! I :! O :! O, 0b100)
          , (Top :! I :! O :! I, 0b101)
          , (Top :! I :! I :! O, 0b110)
          , (Top :! I :! I :! I, 0b111)
          , (Top :! O, 0b1000)
          , (Top :! I, 0b1001)
          , (Top, 0b1010)
          ]

    forM_ examples \(input, expected) ->
      it ("embeds " ++ show input ++ " as " ++ show expected) do
        embed input `shouldBe` expected

    forM_ examples \(Just -> expected, input) ->
      it ("selects " ++ show input ++ " as " ++ show expected) do
        select input `shouldBe` expected

    it "selects 0b1011 as Nothing" do
      select 0b1011 `shouldBe` (Nothing :: Maybe (FBinary ObIOII))

  describe "Show instance" do
    let examples =
          [ (Top :! O :! O :! O, "Top :! O :! O :! O")
          , (Top :! O :! O :! I, "Top :! O :! O :! I")
          , (Top :! O :! I :! O, "Top :! O :! I :! O")
          , (Top :! O :! I :! I, "Top :! O :! I :! I")
          , (Top :! I :! O :! O, "Top :! I :! O :! O")
          , (Top :! I :! O :! I, "Top :! I :! O :! I")
          , (Top :! I :! I :! O, "Top :! I :! I :! O")
          , (Top :! I :! I :! I, "Top :! I :! I :! I")
          , (Top :! O, "Top :! O")
          , (Top :! I, "Top :! I")
          , (Top, "Top")
          ]
    forM_ examples \(input, expected) ->
      it ("renders `" ++ expected ++ "` as " ++ show expected) do
        show input `shouldBe` expected

  describe "Ord instance" do
    it "confirms 0s before 1s" do
      compare (Top :! O :! O :! O) (Top :! I :! I :! I) `shouldBe` LT

    it "confirms longer chains before shorter chains" do
      compare (Top :! I :! I :! I) (Top :! O) `shouldBe` LT

    it "confirms Top is maximum" do
      compare (Top :! O) Top  `shouldBe` LT
      
  describe "Enum instance" do
    describe "safeSucc" do
      it "updates O to I" do
        safeSucc (Top :! O :! O :! O :: FBinary ObIOII) `shouldBe` Just (Top :! O :! O :! I)

      it "handles carries" do
        safeSucc (Top :! O :! I :! I :: FBinary ObIOII) `shouldBe` Just (Top :! I :! O :! O)

      it "handles transitions to shorter chains" do
        safeSucc (Top :! I :! I :! I :: FBinary ObIOII) `shouldBe` Just (Top :! O)

      it "handles the maximum" do
        safeSucc (Top :: FBinary ObIOII) `shouldBe` Nothing

    describe "safePred" do
      it "updates I to O" do
        safePred (Top :! O :! O :! I :: FBinary ObIOII) `shouldBe` Just (Top :! O :! O :! O)

      it "handles borrows" do
        safePred (Top :! I :! O :! O :: FBinary ObIOII) `shouldBe` Just (Top :! O :! I :! I)

      it "handles transitions to longer chains" do
        safePred (Top :! O :: FBinary ObIOII) `shouldBe` Just (Top :! I :! I :! I)

      it "handles the minimum" do
        safePred (Top :! O :! O :! O :: FBinary ObIOII) `shouldBe` Nothing

    describe "enumFrom" do
      it "captures the universe of values" do
        enumFrom (Top :! O :! O :! O :: FBinary ObIOII) `shouldBe` 
          [ Top :! O :! O :! O
          , Top :! O :! O :! I
          , Top :! O :! I :! O
          , Top :! O :! I :! I
          , Top :! I :! O :! O
          , Top :! I :! O :! I
          , Top :! I :! I :! O
          , Top :! I :! I :! I
          , Top :! O
          , Top :! I
          , Top
          ]
      it "starts at the proper offset" do
        enumFrom (Top :! I :! I :! O :: FBinary ObIOII) `shouldBe` 
          [ Top :! I :! I :! O
          , Top :! I :! I :! I
          , Top :! O
          , Top :! I
          , Top
          ]

    describe "enumFromTo" do
      it "captures the universe of values" do
        enumFromTo (Top :! O :! O :! O :: FBinary ObIOII) Top `shouldBe` 
          [ Top :! O :! O :! O
          , Top :! O :! O :! I
          , Top :! O :! I :! O
          , Top :! O :! I :! I
          , Top :! I :! O :! O
          , Top :! I :! O :! I
          , Top :! I :! I :! O
          , Top :! I :! I :! I
          , Top :! O
          , Top :! I
          , Top
          ]

      it "starts at the proper offset" do
        enumFromTo (Top :! I :! I :! O :: FBinary ObIOII) Top `shouldBe` 
          [ Top :! I :! I :! O
          , Top :! I :! I :! I
          , Top :! O
          , Top :! I
          , Top
          ]

      it "stops at the proper offset" do
        enumFromTo (Top :! I :! I :! O :: FBinary ObIOII) (Top :! O) `shouldBe` 
          [ Top :! I :! I :! O
          , Top :! I :! I :! I
          , Top :! O
          ]

      it "handles an empty range" do
        enumFromTo (Top :! O) (Top :! I :! I :! O :: FBinary ObIOII) `shouldBe` []

  describe "Bounded instance" do
    it "reports an accurate minimum for odd cardinality" do
      (minBound :: FBinary ObIOII) `shouldBe` (Top :! O :! O :! O)

    it "reports an accurate maximum for odd cardinality" do
      (maxBound :: FBinary ObIOII) `shouldBe` Top

    it "reports an accurate minimum for even cardinality" do
      (minBound :: FBinary (ObIOII ':. 'O)) `shouldBe` (Top :! O :! O :! O :! O)

    it "reports an accurate maximum for even cardinality" do
      (maxBound :: FBinary (ObIOII ':. 'O)) `shouldBe` (Top :! I)
