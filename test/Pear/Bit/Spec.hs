{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module Pear.Bit.Spec where

import Data.GenValidity
import Pear.Bit

import Test.Hspec
import Test.Validity

instance Validity Bit
instance GenValid Bit

bitSpec :: Spec
bitSpec = describe "Bit" do
  eqSpec @Bit
  ordSpec @Bit

