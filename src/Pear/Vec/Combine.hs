{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Pear.Vec.Combine where

import GHC.Types (Constraint)

import Pear.Binary
import Pear.Binary.Finite
import Pear.Finite
import Pear.Opt
import Pear.Two
import Pear.Vec

type Combined :: Binary -> Binary -> Bit -> * -> *
data Combined m n b a = Combined
  { vector :: Vec (Add m n b) a
  , to :: To m n b 
  , fro :: Fro m n b 
  }

type To m n b = FinOf m n b -> Fin (Add m n b)
type Fro m n b = Fin (Add m n b) -> FinOf m n b

type FinOf :: Binary -> Binary -> Bit -> *
data FinOf m n b where
  LHS :: !(Fin m) -> FinOf m n b
  RHS :: !(Fin n) -> FinOf m n b
  CarryBit :: FinOf m n 'I

type HeadBin :: Binary -> Constraint
class HeadBin m where
  snoc :: Fin (HighBits m) -> Bit -> Fin m
  top :: Dict (LowBit m ~ 'I) -> Fin m

instance HeadBin 'Ob where
  snoc = \case
  top = \case

instance HeadBin (bs ':. 'O) where
  snoc = (:!)
  top = \case

instance HeadBin (bs ':. 'I) where
  snoc = (:!)
  top Dict = Top

type Dict :: Constraint -> *
data Dict c where
  Dict :: c => Dict c

-- would be O(1), except for strictness
binaryVec :: Vec m a -> Dict (HeadBin m)
binaryVec Nil = Dict
binaryVec (_ :& Some _) = Dict
binaryVec (_ :& None) = Dict

combine :: Vec m a -> Vec n a -> Combined m n 'O a
combine v v' = combineCarry v v' None

combineCarry :: Vec m a -> Vec n a -> Opt b a -> Combined m n b a
combineCarry v@(binaryVec -> Dict) v'@(binaryVec -> Dict) = combineCarry' v v'

combineCarry' :: (HeadBin m, HeadBin n) => Vec m a -> Vec n a -> Opt b a -> Combined m n b a
combineCarry' Nil Nil None = Combined
  do Nil 
  do \case
  do \case
combineCarry' Nil Nil opt@(Some _) = Combined 
  do Nil :& opt
  do \case CarryBit -> Top
  do \case Top -> CarryBit
combineCarry' Nil (rvec :& ropt) bopt = combineCarryR Nil None rvec ropt bopt
combineCarry' (lvec :& lopt) Nil bopt = combineCarryR lvec lopt Nil None bopt
combineCarry' (lvec :& lopt) (rvec :& ropt) bopt = combineCarryR lvec lopt rvec ropt bopt

combineCarryR :: 
  ( Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b)':. Xor (LowBit m) (LowBit n) b )
  , HeadBin m
  , HeadBin n
  ) =>
  Vec (HighBits m) (Two a) -> Opt (LowBit m) a -> 
  Vec (HighBits n) (Two a) -> Opt (LowBit n) a -> 
  Opt b a -> Combined m n b a
combineCarryR lvec lopt rvec ropt bopt = withCombined
  do (:& xopt)
  do toFor lopt ropt bopt
  do froFor lopt ropt bopt
  do combineCarry lvec rvec copt
  where 
    (copt, xopt) = carry lopt ropt bopt

carry :: Opt x a -> Opt y a -> Opt z a -> (Opt (Carry x y z) (Two a), Opt (Xor x y z) a)
carry None None opt = (None, opt)
carry None opt@(Some _) None = (None, opt)
carry opt@(Some _) None None = (None, opt)
carry (Some la) (Some ra) opt = (Some (la :* ra), opt)
carry (Some la) None (Some ba) = (Some (la :* ba), None)
carry None (Some ra) (Some ba) = (Some (ra :* ba), None)

froFor :: 
  ( Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b)
  , HeadBin m
  , HeadBin n
  ) =>
  Opt (LowBit m) i -> 
  Opt (LowBit n) j -> 
  Opt b k -> 
  Fro (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) -> 
  Fro m n b
froFor None None _ fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> CarryBit
froFor None (Some _) None fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> RHS (top Dict)
froFor (Some _) None None fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
  Top -> LHS (top Dict)
froFor (Some _) (Some _) _ fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> LHS (top Dict)
      I -> RHS (top Dict)
  Top -> CarryBit
froFor (Some _) None (Some _) fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> LHS (top Dict)
      I -> CarryBit
froFor None (Some _) (Some _) fro = \case
  bs :! b -> case fro bs of
    LHS bs -> LHS (snoc bs b)
    RHS bs -> RHS (snoc bs b)
    CarryBit -> case b of
      O -> RHS (top Dict)
      I -> CarryBit

toFor :: 
  (Add m n b ~ (Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b)) =>
  Opt (LowBit m) i -> Opt (LowBit n) j -> Opt b k -> To (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) -> To m n b
toFor None None _ to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  CarryBit -> Top
toFor None (Some _) None to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> Top
toFor (Some _) None None to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> Top
  RHS (bs :! b) -> to (RHS bs) :! b
toFor (Some _) (Some _) _ to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> to CarryBit :! O
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> to CarryBit :! I
  CarryBit -> Top
toFor (Some _) None (Some _) to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  LHS Top -> to CarryBit :! O
  RHS (bs :! b) -> to (RHS bs) :! b
  CarryBit -> to CarryBit :! I
toFor None (Some _) (Some _) to = \case
  LHS (bs :! b) -> to (LHS bs) :! b
  RHS (bs :! b) -> to (RHS bs) :! b
  RHS Top -> to CarryBit :! O
  CarryBit -> to CarryBit :! I

withCombined :: 
  (Vec (Add m' n' b') a' -> Vec (Add m n b) a) ->
  (To m' n' b' -> To m n b) ->
  (Fro m' n' b' -> Fro m n b) ->
  Combined m' n' b' a' -> Combined m n b a
withCombined withVector withTo withFro Combined{vector,to,fro} = Combined
  { vector = withVector vector
  , to = withTo to
  , fro = withFro fro
  }

type LowBit :: Binary -> Bit
type family LowBit m where
  LowBit 'Ob = 'O
  LowBit (bs ':. b) = b

type HighBits :: Binary -> Binary
type family HighBits m where
  HighBits 'Ob = 'Ob
  HighBits (bs ':. b) = bs

type Carry :: Bit -> Bit -> Bit -> Bit
type family Carry x y z where
  Carry 'O 'O b = 'O
  Carry 'O 'I b = b
  Carry 'I 'O b = b
  Carry 'I 'I b = 'I
  
type Xor :: Bit -> Bit -> Bit -> Bit
type family Xor x y z where
  Xor p q q = p
  Xor q p q = p
  Xor q q p = p

type Add :: Binary -> Binary -> Bit -> Binary
type family Add m n b where
  Add 'Ob 'Ob 'O = 'Ob
  Add 'Ob 'Ob 'I = 'Ob ':. 'I
  Add m n b = Add (HighBits m) (HighBits n) (Carry (LowBit m) (LowBit n) b) ':. Xor (LowBit m) (LowBit n) b
