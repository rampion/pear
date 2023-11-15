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

  describe "fromPositive" do
    it "accurately translates 1" do
      fromPositive ObI `shouldBe` 1

    it "accurately translates 2" do
      fromPositive (ObI :. O) `shouldBe` 2

    it "accurately translates 3" do
      fromPositive (ObI :. I) `shouldBe` 3

    it "accurately translates 10" do
      fromPositive (ObI :. O :. I :. O) `shouldBe` 10

  describe "toPositive" do
    it "accurately translates 1" do
      toPositive 1 `shouldBe` Just ObI

    it "accurately translates 2" do
      toPositive 2 `shouldBe` Just (ObI :. O)

    it "accurately translates 3" do
      toPositive 3 `shouldBe` Just (ObI :. I)

    it "accurately translates 10" do
      toPositive 10 `shouldBe` Just (ObI :. O :. I :. O)

    it "returns Nothing for 0" do
      toPositive 0 `shouldBe` Nothing

  describe "literal" do
    it "accurately translates 1" do
      literal @1 `shouldBe` ObI

    it "accurately translates 2" do
      literal @2 `shouldBe` (ObI :. O)

    it "accurately translates 3" do
      literal @3 `shouldBe` (ObI :. I)

    it "accurately translates 10" do
      literal @10 `shouldBe` (ObI :. O :. I :. O)
