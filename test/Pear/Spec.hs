{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Pear()
import Pear.Bit.Spec
import Pear.Binary.Spec

import Test.Hspec

main :: IO ()
main = do
  hspec do
    bitSpec
    binarySpec
