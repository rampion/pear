module Pear.Tree.Spec where

import Test.Hspec
import Pear.Tree
import Pear.Zipper

spec :: Spec
spec = describe "Pear.Tree" do
  describe "instance Show (Tree a)" do
    it "renders a one-element tree correctly" do
      show (Top 'a') `shouldBe` "Top 'a'"

    it "renders a a two-element tree correctly" do
      show (Top ('a' :× 'b') :>- Nothing) `shouldBe` "Top ('a' :× 'b') :>- Nothing"

    it "renders a a three-element tree correctly" do
      show (Top ('a' :× 'b') :>- Just 'c') `shouldBe` "Top ('a' :× 'b') :>- Just 'c'"

    it "renders a a five-element tree correctly" do
      show (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` "Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e'"

  describe "size" do
    it "measures a one-element tree correctly" do
      size (Top 'a') `shouldBe` ObI

    it "measures a two-element tree correctly" do
      size (Top ('a' :× 'b') :>- Nothing) `shouldBe` ObI :. O

    it "measures a three-element tree correctly" do
      size (Top ('a' :× 'b') :>- Just 'c') `shouldBe` ObI :. I

    it "measures a five-element tree correctly" do
      size (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` ObI :. O :. I

  describe "push" do
    it "appends to a one-element tree correctly" do
      push 'b' (Top 'a') `shouldBe` Top ('a' :× 'b') :>- Nothing

    it "appends to a two-element tree correctly" do
      push 'c' (Top ('a' :× 'b') :>- Nothing) `shouldBe` Top ('a' :× 'b') :>- Just 'c'

    it "appends to a five-element tree correctly" do
      push 'f' (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing

    it "appends to a seven-element tree correctly" do
      push 'h' (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g') `shouldBe` Top ((('a' :× 'b') :× ('c' :× 'd')) :× (('e' :× 'f') :× ('g' :× 'h'))) :>- Nothing :>- Nothing :>- Nothing

  describe "pop" do
    it "splits a one-element tree correctly" do
      pop (Top 'a') `shouldBe` (Nothing, 'a')

    it "splits a two-element tree correctly" do
      pop (Top ('a' :× 'b') :>- Nothing) `shouldBe` (Just (Top 'a'), 'b')

    it "splits a five-element tree correctly" do
      pop (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing), 'e')

    it "splits a six-element tree correctly" do
      pop (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing) `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e'), 'f')

    it "splits a eight-element tree correctly" do
      pop (Top ((('a' :× 'b') :× ('c' :× 'd')) :× (('e' :× 'f') :× ('g' :× 'h'))) :>- Nothing :>- Nothing :>- Nothing) `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'), 'h')

  describe "pop2" do
    it "splits a two-element tree correctly" do
      pop2 (Top ('a' :× 'b')) `shouldBe` (Top 'a', 'b')

    it "splits a six-element tree correctly" do
      pop2 (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f')) `shouldBe` (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e', 'f')

    it "splits a eight-element tree correctly" do
      pop2 (Top ((('a' :× 'b') :× ('c' :× 'd')) :× (('e' :× 'f') :× ('g' :× 'h'))) :>- Nothing :>- Nothing) `shouldBe` (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g', 'h')

  describe "instance Zipperable Tree" do
    describe "zipUp" do
      it "rezips a one-element tree correctly" do
        zipUp (Zipper AtTop 'a')
          `shouldBe` Top 'a'

      it "rezips a two-element tree focused on the first element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< 'b', Nothing)) 'a')
          `shouldBe` Top ('a' :× 'b') :>- Nothing

      it "rezips a two-element tree focused on the second element correctly" do
        zipUp (Zipper (AtTop :\ ('a' :> Hole, Nothing)) 'b')
          `shouldBe` Top ('a' :× 'b') :>- Nothing

      it "rezips a three-element tree focused on the first element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< 'b', Just 'c')) 'a')
          `shouldBe` Top ('a' :× 'b') :>- Just 'c'

      it "rezips a three-element tree focused on the second element correctly" do
        zipUp (Zipper (AtTop :\ ('a' :> Hole, Just 'c')) 'b')
          `shouldBe` Top ('a' :× 'b') :>- Just 'c'

      it "rezips a three-element tree focused on the third element correctly" do
        zipUp (Zipper (Top ('a' :× 'b') :\- Hole) 'c')
          `shouldBe` Top ('a' :× 'b') :>- Just 'c'

      it "rezips a four-element tree focused on the first element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ (Hole :< 'b', Nothing)) 'a')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing

      it "rezips a four-element tree focused on the second element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing

      it "rezips a four-element tree focused on the third element correctly" do
        zipUp (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ (Hole :< 'd', Nothing)) 'c')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing

      it "rezips a four-element tree focused on the fourth element correctly" do
        zipUp (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ ('c' :> Hole, Nothing)) 'd')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing

      it "rezips a seven-element tree focused on the first element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ (Hole :< 'b', Just 'g')) 'a')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the second element correctly" do
        zipUp (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ ('a' :> Hole, Just 'g')) 'b')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the third element correctly" do
        zipUp (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ (Hole :< 'd', Just 'g')) 'c')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the fourth element correctly" do
        zipUp (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Just 'g')) 'd')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the fifth element correctly" do
        zipUp (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Just 'g')) 'e')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the sixth element correctly" do
        zipUp (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

      it "rezips a seven-element tree focused on the seventh element correctly" do
        zipUp (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g')
          `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'

    describe "zipDown" do
      it "unzips a one-element tree correctly" do
        zipDown (Top 'a') `shouldBe` Top (Zipper AtTop 'a')

      it "unzips a two-element tree correctly" do
        zipDown (Top ('a' :× 'b') :>- Nothing) `shouldBe` 
          Top (  Zipper (AtTop :\ (Hole :< 'b', Nothing)) 'a' 
              :× Zipper (AtTop :\ ('a' :> Hole, Nothing)) 'b'
              ) :>- Nothing

      it "unzips a three-element tree correctly" do
        zipDown (Top ('a' :× 'b') :>- Just 'c') `shouldBe`
          Top (  Zipper (AtTop :\ (Hole :< 'b', Just 'c')) 'a'
              :× Zipper (AtTop :\ ('a' :> Hole, Just 'c')) 'b'
              )
            :>- Just (Zipper (Top ('a' :× 'b') :\- Hole) 'c')

      it "unzips a four-element tree correctly" do
        zipDown (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing) `shouldBe`
          Top 
            (  (  Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ (Hole :< 'b', Nothing)) 'a'
               :× Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b'
               )
            :× (  Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ (Hole :< 'd', Nothing)) 'c'
               :× Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ ('c' :> Hole, Nothing)) 'd'
               )
            ) :>- Nothing :>- Nothing

      it "unzips a seven-element tree correctly" do
        zipDown (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g') `shouldBe` 
          Top
            (  (  Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ (Hole :< 'b', Just 'g')) 'a'
               :× Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ ('a' :> Hole, Just 'g')) 'b'
               )
            :× (  Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ (Hole :< 'd', Just 'g')) 'c'
               :× Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Just 'g')) 'd'
               )
            )
            :>- Just
              (  Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Just 'g')) 'e'
              :× Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f'
              )
            :>- Just
              ( Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g'
              )