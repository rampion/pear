{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Base₂()
import Base₂.Bit.Test
import Base₂.Binary.Test

-- import Test.DocTest
import Test.Hspec

main :: IO ()
main = do
  hspec do
    bitSpec
    binarySpec

  -- doctest $ words "-pgmL markdown-unlit" -- add .hs and .lhs files?
