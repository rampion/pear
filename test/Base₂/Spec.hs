{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Base₂()
import Base₂.Bit.Spec
import Base₂.Binary.Spec

import Test.Hspec

main :: IO ()
main = do
  hspec do
    bitSpec
    binarySpec
