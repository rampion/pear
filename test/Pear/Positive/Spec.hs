module Pear.Positive.Spec where

import Test.Hspec
import Pear.Positive

spec :: Spec
spec = describe "Pear.Positive" do
  describe "Show" do
    it "renders 1 correctly" do
      show ObI `shouldBe` "ObI"

    it "renders 2 correctly" do
      show (ObI :. O) `shouldBe` "ObI :. O"

    it "renders 5 correctly" do
      show (ObI :. O :. I) `shouldBe` "ObI :. O :. I"

  describe "toNatural" do
    it "accurately translates 1" do
      toNatural ObI `shouldBe` 1

    it "accurately translates 2" do
      toNatural (ObI :. O) `shouldBe` 2

    it "accurately translates 3" do
      toNatural (ObI :. I) `shouldBe` 3

    it "accurately translates 10" do
      toNatural (ObI :. O :. I :. O) `shouldBe` 10

  describe "fromNatural" do
    it "accurately translates 1" do
      fromNatural 1 `shouldBe` Just ObI

    it "accurately translates 2" do
      fromNatural 2 `shouldBe` Just (ObI :. O)

    it "accurately translates 3" do
      fromNatural 3 `shouldBe` Just (ObI :. I)

    it "accurately translates 10" do
      fromNatural 10 `shouldBe` Just (ObI :. O :. I :. O)

    it "returns Nothing for 0" do
      fromNatural 0 `shouldBe` Nothing

  describe "literal" do
    it "accurately translates 1" do
      literal @1 `shouldBe` ObI

    it "accurately translates 2" do
      literal @2 `shouldBe` (ObI :. O)

    it "accurately translates 3" do
      literal @3 `shouldBe` (ObI :. I)

    it "accurately translates 10" do
      literal @10 `shouldBe` (ObI :. O :. I :. O)
