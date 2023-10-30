module Main where

import Pear.Positive.Spec qualified as Positive
import Pear.Tree.Spec qualified as Tree
import Test.Hspec

main :: IO ()
main = do
  hspec do
    Tree.spec
    Positive.spec
