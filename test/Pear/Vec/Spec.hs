{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Vec.Spec
  ( vecSpec
  )
  where

import Pear.Binary
import Pear.Opt
import Pear.Two
import Pear.Vec
import Test.Hspec

type ObIOII = 'Ob ':. 'I ':. 'O ':. 'I ':. 'I

vecSpec :: Spec
vecSpec = describe "Vec" do
  let v :: Vec ObIOII Int
      v = Nil :& 
          Some (((0 :* 1) :* (2 :* 3)) :* ((4 :* 5) :* (6 :* 7))) :& 
          None :& 
          Some (8 :* 9) :&
          Some 10

  describe "shiftr" do
    it "moves all the elements one position right" do
      shiftr (-1) v `shouldBe`
        ( Nil :& 
          Some (((negate 1 :* 0) :* (1 :* 2)) :* ((3 :* 4) :* (5 :* 6))) :& 
          None :& 
          Some (7 :* 8) :&
          Some 9
        , 10
        )
      
  describe "shiftl" do
    it "moves all the eleements one position left" do
      shiftl v 11 `shouldBe`
        ( 0
        , Nil :& 
          Some (((1 :* 2) :* (3 :* 4)) :* ((5 :* 6) :* (7 :* 8))) :& 
          None :& 
          Some (9 :* 10) :&
          Some 11
        )
      
  describe "rotr" do
    it "moves all the elements one position right, circularly" do
      rotr v `shouldBe`
        ( Nil :& 
          Some (((10 :* 0) :* (1 :* 2)) :* ((3 :* 4) :* (5 :* 6))) :& 
          None :& 
          Some (7 :* 8) :&
          Some 9
        )
      
  describe "rotl" do
    it "moves all the eleements one position left, circularly" do
      rotl v `shouldBe`
        ( Nil :& 
          Some (((1 :* 2) :* (3 :* 4)) :* ((5 :* 6) :* (7 :* 8))) :& 
          None :& 
          Some (9 :* 10) :&
          Some 0
        )
