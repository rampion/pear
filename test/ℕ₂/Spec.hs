{-# LANGUAGE BlockArguments #-}
module Main (main) where

import ℕ₂()
import ℕ₂.Bit.Spec
import ℕ₂.Binary.Spec

import Test.Hspec

main :: IO ()
main = do
  hspec do
    bitSpec
    binarySpec
