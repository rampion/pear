module Pear.Tree.Spec where

import Test.Hspec
import Pear.Tree

spec :: Spec
spec = describe "Pear.Tree" do
  describe "Show" do
    it "renders a tree with one element correctly" do
      show (Top 'a') `shouldBe` "Top 'a'"

    it "renders a a tree with two elements correctly" do
      show (Top ('a' :× 'b') :>- Nothing) `shouldBe` "Top ('a' :× 'b') :>- Nothing"

    it "renders a a tree with three elements correctly" do
      show (Top ('a' :× 'b') :>- Just 'c') `shouldBe` "Top ('a' :× 'b') :>- Just 'c'"

    it "renders a a tree with five elements correctly" do
      show (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` "Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e'"

  describe "size" do
    it "measures a tree with one element correctly" do
      size (Top 'a') `shouldBe` ObI

    it "measures a a tree with two elements correctly" do
      size (Top ('a' :× 'b') :>- Nothing) `shouldBe` ObI :. O

    it "measures a a tree with three elements correctly" do
      size (Top ('a' :× 'b') :>- Just 'c') `shouldBe` ObI :. I

    it "measures a a tree with five elements correctly" do
      size (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` ObI :. O :. I

  describe "push" do
    it "appends to a tree with one element correctly" do
      push 'b' (Top 'a') `shouldBe` Top ('a' :× 'b') :>- Nothing

    it "appends to a tree with two elements correctly" do
      push 'c' (Top ('a' :× 'b') :>- Nothing) `shouldBe` Top ('a' :× 'b') :>- Just 'c'

    it "appends to a tree with five elements correctly" do
      push 'f' (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing

    it "appends to a tree with seven elements correctly" do
      push 'h' (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g') `shouldBe` Top ((('a' :× 'b') :× ('c' :× 'd')) :× (('e' :× 'f') :× ('g' :× 'h'))) :>- Nothing :>- Nothing :>- Nothing

