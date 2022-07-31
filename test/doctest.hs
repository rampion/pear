{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest $ words "-pgmL markdown-unlit -optL haskell+example src"
