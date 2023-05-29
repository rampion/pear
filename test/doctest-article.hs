{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest 
  [ "-pgmL markdown-unlit"
  , "-optL haskell+example"
  , "-interactive-print=Text.Show.Pretty.pPrint"
  , "doc/Article.lhs"
  ]
