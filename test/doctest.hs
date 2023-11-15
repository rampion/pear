{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = doctest do flags <> pkgs <> module_sources
