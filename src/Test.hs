{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Data.Natural.Binary.Type.Test
import Test.Hspec

main :: IO ()
main = hspec do
  bitSpec
  binarySpec
