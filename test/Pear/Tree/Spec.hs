module Pear.Tree.Spec where

import Prelude hiding (reverse, span, splitAt)
import Data.Foldable qualified as Foldable
import Pear.Tree
import Test.Hspec

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
      push (Top 'a') 'b' `shouldBe` Top ('a' :× 'b') :>- Nothing

    it "appends to a two-element tree correctly" do
      push (Top ('a' :× 'b') :>- Nothing) 'c' `shouldBe` Top ('a' :× 'b') :>- Just 'c'

    it "appends to a five-element tree correctly" do
      push (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e') 'f' `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing

    it "appends to a seven-element tree correctly" do
      push (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g') 'h' `shouldBe` Top ((('a' :× 'b') :× ('c' :× 'd')) :× (('e' :× 'f') :× ('g' :× 'h'))) :>- Nothing :>- Nothing :>- Nothing

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

    describe "zipForward" do
      it "moves from the first to the second element of a two-element tree" do
        zipForward (Zipper (AtTop :\ (Hole :< 'b', Nothing)) 'a') `shouldBe`
          Just (Zipper (AtTop :\ ('a' :> Hole, Nothing)) 'b')
          
      it "moves from the second to the third element of a three-element tree" do
        zipForward (Zipper (AtTop :\ ('a' :> Hole, Just 'c')) 'b') `shouldBe`
          Just (Zipper (Top ('a' :× 'b') :\- Hole) 'c')
          
      it "moves from the first to the second element of a four-element tree" do
        zipForward (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ (Hole :< 'b', Nothing)) 'a') `shouldBe`
          Just (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b')

      it "moves from the second to the third element of a four-element tree" do
        zipForward (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b') `shouldBe`
          Just (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ (Hole :< 'd', Nothing)) 'c')

      it "moves from the fourth to the fifth element of a five-element tree" do
        zipForward (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ ('c' :> Hole, Just 'e')) 'd') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :\- Hole) 'e')

      it "moves from the fourth to the fifth element of a six-element tree" do
        zipForward (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Nothing)) 'd') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Nothing)) 'e')

      it "moves from the fifth to the sixth element of a six-element tree" do
        zipForward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Nothing)) 'e') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Nothing)) 'f')

      it "moves from the six to the seventh element of a seven-element tree" do
        zipForward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g')

    describe "zipBackward" do
      it "moves from the second to the first element of a two-element tree" do
        zipBackward (Zipper (AtTop :\ ('a' :> Hole, Nothing)) 'b') `shouldBe`
          Just (Zipper (AtTop :\ (Hole :< 'b', Nothing)) 'a')
          
      it "moves from the third to the second element of a three-element tree" do
        zipBackward (Zipper (Top ('a' :× 'b') :\- Hole) 'c') `shouldBe`
          Just (Zipper (AtTop :\ ('a' :> Hole, Just 'c')) 'b')
          
      it "moves from the second to the first element of a four-element tree" do
        zipBackward (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b') `shouldBe`
          Just (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ (Hole :< 'b', Nothing)) 'a')

      it "moves from the third to the second element of a four-element tree" do
        zipBackward (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ (Hole :< 'd', Nothing)) 'c') `shouldBe`
          Just (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b')

      it "moves from the fifth to the fourth element of a five-element tree" do
        zipBackward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :\- Hole) 'e') `shouldBe`
          Just (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ ('c' :> Hole, Just 'e')) 'd')

      it "moves from the fifth to the fourth element of a six-element tree" do
        zipBackward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Nothing)) 'e') `shouldBe`
          Just (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Nothing)) 'd')

      it "moves from the sixth to the fifth element of a six-element tree" do
        zipBackward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Nothing)) 'f') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Nothing)) 'e')

      it "moves from the seventh to the six element of a seven-element tree" do
        zipBackward (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g') `shouldBe`
          Just (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f')

  describe "instance Traversable (Zipper Tree)" do
    it "traverses a seven-element zipper focused on the first element in the correct order" do
      Foldable.toList (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ (Hole :< 'b', Just 'g')) 'a')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the second element in the correct order" do
      Foldable.toList (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ ('a' :> Hole, Just 'g')) 'b')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the third element in the correct order" do
      Foldable.toList (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ (Hole :< 'd', Just 'g')) 'c')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the fourth element in the correct order" do
      Foldable.toList (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Just 'g')) 'd')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the fifth element in the correct order" do
      Foldable.toList (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Just 'g')) 'e')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the sixth element in the correct order" do
      Foldable.toList (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f')
        `shouldBe` "abcdefg"

    it "traverses a seven-element zipper focused on the seventh element in the correct order" do
      Foldable.toList (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g')
        `shouldBe` "abcdefg"

  describe "instance Semigroup Tree" do
    it "concatenates the trees" do
      (Top 'a' <> Top 'b' <> Top 'c') `shouldBe`
        (Top ('a' :× 'b') :>- Just 'c')
        
    it "is associative" do
      (Top 'a' <> (Top 'b' <> Top 'c')) `shouldBe`
        ((Top 'a' <> Top 'b') <> Top 'c')

  describe "reverse" do
    it "reverses a four element tree" do
      reverse (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing)
        `shouldBe` Top (('d' :× 'c') :× ('b' :× 'a')) :>- Nothing :>- Nothing

    it "reverses a seven element tree" do
      reverse (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` Top (('g' :× 'f') :× ('e' :× 'd')) :>- Just ('c' :× 'b') :>- Just 'a'
    
  describe "partition" do
    it "partitions a list of seven elements correctly" do
      let rejectVowel x 
            | x `elem` "aeiouyAEIOUY" = Left x
            | otherwise = Right x
      partition rejectVowel 
        do Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g'
        `shouldBe` 
          These
            do Top ('a' :× 'e') :>- Nothing
            do Top (('b' :× 'c') :× ('d' :× 'f')) :>- Nothing :>- Just 'g'

  describe "span" do
    it "breaks two trees before the first false test" do 
        span (< 'g') (Top (('a' :× 'b') :× ('c' :× 'g')) :>- Just ('f' :× 'e') :>- Just 'd')
          `shouldBe`
            These
              do Top ('a' :× 'b') :>- Just 'c'
              do Top (('g' :× 'f') :× ('e' :× 'd')) :>- Nothing :>- Nothing
      
    it "returns the entire tree as This if all elements are true" do 
        span (< 'h') (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
          `shouldBe`
            This (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
      
    it "returns the entire tree as That if the first element is false" do 
        span (> 'a') (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
          `shouldBe`
            That (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')

  describe "unshift" do
    it "prepends an element to a singleton tree" do
      unshift 'c' (Top 'd')
        `shouldBe` Top ('c' :× 'd') :>- Nothing
    it "prepends an element to a three-element tree" do
      unshift 'a' (Top ('b' :× 'c') :>- Just 'd')
        `shouldBe` Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing

  describe "shift" do
    it "removes the first element from a singleton tree" do
      shift (Top 'd')
        `shouldBe` ('d', Nothing)

    it "removes the first element from a four-element tree" do
      shift (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing)
        `shouldBe` ('a', Just (Top ('b' :× 'c') :>- Just 'd'))

  describe "zipSplit" do
    describe "for a zipper of a one-element tree" do
      it "splits into the proper trees when focused on the only element" do
        zipSplit (Zipper AtTop 'a')
          `shouldBe` (Nothing, Top 'a')

    describe "for a zipper of a two-element tree" do
      it "splits into the proper trees when focused on the first element" do
        zipSplit (Zipper (AtTop :\ (Hole :< 'b', Nothing)) 'a')
          `shouldBe` (Nothing, Top ('a' :× 'b') :>- Nothing)

      it "splits into the proper trees when focused on the second element" do
        zipSplit (Zipper (AtTop :\ ('a' :> Hole, Nothing)) 'b')
          `shouldBe` (Just (Top 'a'), Top 'b')

    describe "for a zipper of a three-element tree" do
      it "splits into the proper trees when focused on the first element" do
        zipSplit (Zipper (AtTop :\ (Hole :< 'b', Just 'c')) 'a')
          `shouldBe` (Nothing, Top ('a' :× 'b') :>- Just 'c')

      it "splits into the proper trees when focused on the second element" do
        zipSplit (Zipper (AtTop :\ ('a' :> Hole, Just 'c')) 'b')
          `shouldBe` (Just (Top 'a'), Top ('b' :× 'c') :>- Nothing)

      it "splits into the proper trees when focused on the third element" do
        zipSplit (Zipper (Top ('a' :× 'b') :\- Hole) 'c')
          `shouldBe` (Just (Top ('a' :× 'b') :>- Nothing), Top 'c')

    describe "for a zipper of a four-element tree" do
      it "splits into the proper trees when focused on the first element" do
        zipSplit (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ (Hole :< 'b', Nothing)) 'a')
          `shouldBe` (Nothing, Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing)

      it "splits into the proper trees when focused on the second element" do
        zipSplit (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Nothing) :\ ('a' :> Hole, Nothing)) 'b')
          `shouldBe` (Just (Top 'a'), Top ('b' :× 'c') :>- Just 'd')

      it "splits into the proper trees when focused on the third element" do
        zipSplit (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ (Hole :< 'd', Nothing)) 'c')
          `shouldBe` (Just (Top ('a' :× 'b') :>- Nothing), Top ('c' :× 'd') :>- Nothing)

      it "splits into the proper trees when focused on the fourth element" do
        zipSplit (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Nothing) :\ ('c' :> Hole, Nothing)) 'd')
          `shouldBe` (Just (Top ('a' :× 'b') :>- Just 'c'), Top 'd')

    describe "for a zipper of a seven-element tree" do
      it "splits into the proper trees when focused on the first element" do
        zipSplit (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ (Hole :< 'b', Just 'g')) 'a')
          `shouldBe` (Nothing, Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')

      it "splits into the proper trees when focused on the second element" do
        zipSplit (Zipper (AtTop :\ (Hole :< ('c' :× 'd'), Just ('e' :× 'f')) :\ ('a' :> Hole, Just 'g')) 'b')
          `shouldBe` (Just (Top 'a'), Top (('b' :× 'c') :× ('d' :× 'e')) :>- Just ('f' :× 'g') :>- Nothing)

      it "splits into the proper trees when focused on the third element" do
        zipSplit (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ (Hole :< 'd', Just 'g')) 'c')
          `shouldBe` (Just (Top ('a' :× 'b') :>- Nothing), Top (('c' :× 'd') :× ('e' :× 'f')) :>- Nothing :>- Just 'g')

      it "splits into the proper trees when focused on the fourth element" do
        zipSplit (Zipper (AtTop :\ (('a' :× 'b') :> Hole, Just ('e' :× 'f')) :\ ('c' :> Hole, Just 'g')) 'd')
          `shouldBe` (Just (Top ('a' :× 'b') :>- Just 'c'), Top (('d' :× 'e') :× ('f' :× 'g')) :>- Nothing :>- Nothing)

      it "splits into the proper trees when focused on the fifth element" do
        zipSplit (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ (Hole :< 'f', Just 'g')) 'e')
          `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing), Top ('e' :× 'f') :>- Just 'g')

      it "splits into the proper trees when focused on the sixth element" do
        zipSplit (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :\- Hole :\ ('e' :> Hole, Just 'g')) 'f')
          `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e'), Top ('f' :× 'g') :>- Nothing)

      it "splits into the proper trees when focused on the seventh element" do
        zipSplit (Zipper (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :\- Hole) 'g')
          `shouldBe` (Just (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing), Top 'g')
{-
  xdescribe "splitAt" do
    it "can split a seven-element tree into a one-element tree and a six-element tree" do
      splitAt ObI (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top 'a'
            , Top (('b' :× 'c') :× ('d' :× 'e')) :>- Just ('f' :× 'g') :>- Nothing
            )

    it "can split a seven-element tree into a two-element tree and a five-element tree" do
      splitAt (ObI :. O) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top ('a' :× 'b') :>- Nothing
            , Top (('c' :× 'd') :× ('e' :× 'f')) :>- Nothing :>- Just 'g'
            )

    it "can split a seven-element tree into a three-element tree and a four-element tree" do
      splitAt (ObI :. I) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top ('a' :× 'b') :>- Just 'c'
            , Top (('d' :× 'e') :× ('f' :× 'g')) :>- Nothing :>- Nothing
            )

    it "can split a seven-element tree into a four-element tree and a three-element tree" do
      splitAt (ObI :. O :. O) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Nothing
            , Top ('e' :× 'f') :>- Just 'g'
            )

    it "can split a seven-element tree into a five-element tree and a two-element tree" do
      splitAt (ObI :. O :. I) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing :>- Just 'e'
            , Top ('f' :× 'g') :>- Nothing
            )

    it "can split a seven-element tree into a six-element tree and a one-element tree" do
      splitAt (ObI :. I :. O) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` 
          Just 
            ( Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Nothing
            , Top 'g'
            )

    it "returns Nothing when given a size larger the tree" do
      splitAt (ObI :. I :. I) (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Just ('e' :× 'f') :>- Just 'g')
        `shouldBe` Nothing
-}
