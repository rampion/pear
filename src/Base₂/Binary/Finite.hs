{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Base₂.Binary.Finite where

-- import Data.Bits
import Base₂.Bit
import Base₂.Bit.Singleton
import Base₂.Binary
import Base₂.Finite
import Base₂.Function
import Base₂.Singleton
import Base₂.Singleton.Known
import Base₂.Binary.Singleton

type FBinary :: Binary -> *
data FBinary cardinality where
  (:!) :: !(FBinary bs) -> !Bit -> FBinary (bs ':. b)
  Top :: FBinary (bs ':. 'I)

infixl 4 :!

{-
instance Show (FBinary n)
instance Enum (FBinary n)
instance Eq (FBinary n)
instance Ord (FBinary n)
instance Bits (FBinary n)
instance Num (FBinary n)
instance Real (FBinary n)
instance Integral (FBinary n)
-}
type instance Fin_ Binary = FBinary

instance Finite FBinary where
  embedding = Function { image = image' sing, preimage = preimage' Nothing Just sing } where
    image' :: forall (t :: Binary). Sing t -> Fin t -> Binary
    image' SOb = \case
    image' (ts ::. t) = 
      let img = image' ts
          bs = demote ts
      in case t of
        SO -> \case
          is :! i -> img is :. i
        SI -> \case
          Top -> bs :. O
          is :! i -> img is :. i

    preimage' :: forall r (t :: Binary). Maybe r -> (Fin t -> Maybe r) -> Sing t -> Binary -> Maybe r
    preimage' eq lt (ts ::. t) = \case
      Push bs b -> preimage'
        do case (t, b) of
            (SO, I) -> Nothing
            (SI, O) -> lt Top
            _       -> eq
        do lt . (:! b)
        do ts
        do bs
    preimage' eq _ SOb = \case
      Zero  -> eq
      _     -> Nothing
