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

