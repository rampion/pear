{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Tree where

import Numeric.Natural
import Pear.Pair as Pair
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.These

data PearTree a
  = Top a
  | PearTree (Pair a) :>- Maybe a
  deriving (Eq, Functor, Foldable, Traversable)

infix 4 :>-

instance Show a => Show (PearTree a) where
  showsPrec p = showParen (p >= 4) . showsTree where
    showsTree :: forall a. Show a => PearTree a -> ShowS
    showsTree = \case
      Top a -> showString "Top " . showsPrec 10 a
      t :>- ma -> showsTree t . showString " :>- " . showsPrec 4 ma

newtype a ∊ t = Selected { withSelected :: forall f. Functor f => (a -> f a) -> f t }

at :: forall a. Natural -> PearTree a -> Maybe (a ∊ PearTree a)
at = loop id id Nothing where
  loop :: forall aⁿ. 
    (forall f. Functor f => (a -> f a) -> aⁿ -> f aⁿ) -> 
    (PearTree aⁿ -> PearTree a) -> Maybe (a ∊ PearTree a) -> 
    Natural -> PearTree aⁿ -> Maybe (a ∊ PearTree a)
  loop lens wrap next ix = \case
    ta²ⁿ :>- maⁿ -> 
      let here :: Maybe (a ∊ PearTree a)
          here = maⁿ <&> \aⁿ -> Selected \f -> wrap . (ta²ⁿ :>-) . Just <$> lens f aⁿ
          (q,r) = ix `quotRem` 2
      in 
      if r == 0
        then loop (Pair.at O . lens) (wrap . (:>- maⁿ)) (here <|> next) q ta²ⁿ
        else loop (Pair.at I . lens) (wrap . (:>- maⁿ)) (here *> next) q ta²ⁿ
    Top aⁿ -> case ix of
      0 -> Just do Selected \f -> wrap . Top <$> lens f aⁿ
      1 -> next
      _ -> Nothing

push :: a -> PearTree a -> PearTree a
push a₁ = \case
  ta² :>- Just a₀ -> push (a₀ :× a₁) ta² :>- Nothing
  ta² :>- Nothing -> ta² :>- Just a₁
  Top a₀          -> Top (a₀ :× a₁) :>- Nothing

pop :: PearTree a -> (Maybe (PearTree a), a)
pop = \case
  ta² :>- Nothing  -> case pop ta² of
    (Nothing, a₀ :× a₁) -> (Just (Top a₀), a₁)
    (Just ta², a₀ :× a₁) -> (Just (ta² :>- Just a₀), a₁)
  ta² :>- Just a  -> (Just (ta² :>- Nothing), a)
  Top a -> (Nothing, a)

size :: PearTree a -> Positive
size = loop 1 0 where
  loop !n !t = \case
    Top _ -> t + n
    ta² :>- ma -> loop (2*n) (t + maybe 0 (const n) ma) ta²

fuse :: PearTree a -> PearTree a -> PearTree a
fuse = \taₗ taᵣ -> loop taₗ taᵣ Nothing where
  -- fuse with carry
  loop :: PearTree a -> PearTree a -> Maybe a -> PearTree a
  loop (ta²ₗ :>- maₗ) (ta²ᵣ :>- maᵣ) ma_ = pack maₗ maᵣ ma_ \ma² ma -> 
    loop ta²ₗ ta²ᵣ ma² :>- ma
  loop (ta²ₗ :>- maₗ) (Top aᵣ) ma_ = pack maₗ (Just aᵣ) ma_ \ma² ma -> 
    maybe id push ma² ta²ₗ :>- ma
  loop (Top aₗ) (ta²ᵣ :>- maᵣ) ma_ = pack (Just aₗ) maᵣ ma_ \ma² ma ->
    maybe id push ma² ta²ᵣ :>- ma
  loop (Top aₗ) (Top aᵣ) ma_ = Top (aₗ :× aᵣ) :>- ma_

  -- pack 0-3 elements into a (Maybe (Pair a), Maybe a)
  pack :: Maybe a -> Maybe a -> Maybe a -> (Maybe (Pair a) -> Maybe a  -> r) -> r
  pack (Just a₀) (Just a₁) (Just a₂) k = k (Just (a₀ :× a₁)) (Just a₂)

  pack (Just a₀) (Just a₁) Nothing k = k (Just (a₀ :× a₁)) Nothing
  pack (Just a₀) Nothing (Just a₂) k = k (Just (a₀ :× a₂)) Nothing
  pack Nothing (Just a₁) (Just a₂) k = k (Just (a₁ :× a₂)) Nothing

  pack (Just a₀) Nothing Nothing k = k Nothing (Just a₀)
  pack Nothing (Just a₁) Nothing k = k Nothing (Just a₁)
  pack Nothing Nothing (Just a₂) k = k Nothing (Just a₂)

  pack Nothing Nothing Nothing k = k Nothing Nothing

-- opposite of fuse
--
--  fiss n ta  = Just (taₗ, taᵣ)
--    if and only if
--      size taₗ + size taᵣ = size ta
--      size taᵣ = n
--
--  fiss (size taᵣ) (fuse taₗ taᵣ) = Just (taₗ, taᵣ)
--
--  fiss n ta = Just (taₗ, taᵣ) <=> 0 < n = size taᵣ < size ta
--
--

  -- fizz with no borrow
  loop :: Natural -> PearTree a -> (Natural -> r) -> r -> r -> (PearTree a -> PearTree a -> r) -> r
  loop n ta@(Top _) neither this that _these = case compare n 1 of
    LT -> this
    EQ -> that
    GT -> neither 1
    
  loop n ta@(ta² :>- ma) neither this that these = case n `quotRem` 2 of
    (0, 0) -> this
    (q, 0) -> loop q ta² 
      do \!p -> neither (p * 2 + length ma)
      do this
      do case ma of
            Nothing -> that
            Just aₗ  -> these (Top aₗ) ta²ₗ
      do \ta²ₗ ta²ᵣ -> these (ta²ₗ :>- ma) (ta²ᵣ :>- Nothing)
    (0, 1) -> case (ma, pop ta²) of
      (Just aᵣ, _) -> these (ta² :>- Nothing) (Top aᵣ)
      (_, (Nothing, (aₗ, aᵣ))) -> these (Top aₗ) (Top aᵣ)
      (_, (Just ta²ₗ, (aₗ, aᵣ))) -> these (ta²ₗ :>- Just aₗ) (Top aᵣ)
    (q, 1) -> case ma of
      Just aᵣ -> loop q ta² 
        do \!p -> neither (p * 2 + 1)
        do these (ta²ₗ :>- Nothing) (Top aᵣ)
        do that
        do \ta²ₗ ta²ᵣ -> these (ta²ₗ :>- Nothing) (ta²ᵣ :>- Just aᵣ)
      Nothing -> borrow q ta² 
        do \!p -> neither (p * 2)
        do \taₗ aᵣ -> these taₗ (Top aᵣ)
        do \taₗ ta²ᵣ aᵣ -> these taₗ (ta²ᵣ :>- aᵣ)

  -- fizz with no borrow
  borrow :: Natural -> PearTree (Pair a) -> (Natural -> r) -> (PearTree a -> a -> r) -> (PearTree a -> PearTree (Pair a) -> a -> r) -> r
  borrow n (Top (aₗ :× aᵣ)) tooSmall top _offshoot 
    | n == 0 = top (Top aₗ) aᵣ
    | otherwise = tooSmall 1
  borrow n (ta⁴ :>- ma²) tooSmall top offshoot = case n `quotRem` 2 of
    (0,0) -> case ma² of
      Just (aₗ :× aᵣ) -> top (ta⁴ :>- Nothing :>- Just aₗ) aᵣ
      Nothing -> borrow 0 ta⁴ 
        do \!p -> tooSmall (p * 2)
        do \ta²ₗ (aₗ :× aᵣ) -> top (ta²ₗ :>- Just aₗ) aᵣ
        do \ta²ₗ ta⁴ᵣ (aₗ :× aᵣ) -> offshoot (ta²ₗ :>- Just aₗ) (ta⁴ :>- Nothing) aᵣ
    (q, 0) -> case ma² of
      Just (aₗ :× aᵣ) -> loop q ta⁴ 
        do \!p -> tooSmall (p * 2 + 1)
        do top (ta⁴ :>- Nothing :>- aₗ) aᵣ
        do offshoot (Top aₗ) (ta⁴ :>- Nothing) aᵣ
        do \ta²ₗ ta²ᵣ -> offshoot (ta²ₗ :>- Just aₗ) ta²ᵣ aᵣ
      Nothing -> borrow q ta⁴
        do \!p -> tooSmall (p * 2)
        do \ta²ₗ (aₗ :× aᵣ) -> top (ta²ₗ :>- Just aₗ) aᵣ
        do \ta²ₗ ta⁴ᵣ (aₗ :× aᵣ) -> offshoot (ta²ₗ :>- Just aₗ) (ta⁴ :>- Nothing) aᵣ
    

    
  fissile :: PearTree a -> Natural -> Either (PearTree a -> PearTree a -> r) (PearTree a -> PearTree (Pair a) -> a -> r)



  unpear n ta@(Top _) lt eq gt = ta & case compare n 1 of
    LT -> lt
    EQ -> eq
    GT -> gt
  unpear (q,1) (ta² :>- Nothing) lt eq gt = 
    borrow q ta² 
      do \ta²ᵣ (akkkk



fiss = \n -> \case
  ta² :>- Just a -> \k -> loop q ta² \ta²ₗ ta²ᵣ ->
    if r == 1 
      then k (ta²ₗ :>- Nothing) (ta²ᵣ :>- Just a)
      else k (ta²ₗ :>- Just a) (ta²ᵣ :>- Nothing)
  ta² :>- Nothing -> \k ->
    if r == 1 
      then borrow q ta² \ta²ₗ ta²ᵣ a -> k (ta²ₗ :>- Nothing) (ta²ᵣ :>- Just a)
      else loop q ta² \ta²ₗ ta²ᵣ -> k (ta²ₗ :>- Nothing) (ta²ᵣ :>- Nothing)
  Top a 
  loop


  loop :: Natural -> PearTree (Pair a) -> Maybe a -> Maybe (PearTree a, PearTree a)
  loop n ta² ma k 


  ta@(Top _) -> 

fiss 0 ta = (Just ta, Nothing)
fiss 1 ta@(Top _) = (Nothing, Just ta)
fiss _ (Top _) = (Nothing, Nothing)
fiss (q, 0) (ta² :>- ma) = case fiss q ta² of
  let ~(mta²ₗ, mta²ᵣ) = fiss q ta² 
  in 
  (mta²ₗ <&> (:>- ma) <|> ma <&> Top, mta²ᵣ)
fiss (q, 1) ta = 
  (pop ta)

case fiss q ta² of
  let ~(mta²ₗ, mta²ᵣ) = fiss q ta² 
  in 
  (mta²ₗ <&> (:>- ma) <|> ma <&> Top, mta²ᵣ)


fiss :: Positive -> PearTree a -> Maybe (PearTree a, PearTree a)
fiss = \n ta -> loop n ta (,) where
  loop :: Positive -> PearTree a -> (PearTree a -> PearTree a -> r) -> Maybe r
  loop n ta k = case n `quotRem` 2  of
    (0, 1) -> 
      let (mtaₗ, aᵣ) = pop ta 
      in mtaₗ <&> \taₗ -> (taₗ, Top aᵣ)
    (q, 1) -> case pop ta of
      (Just (ta² :>- maₗ), aᵣ) ->
        (<|>)
          do loop q ta² \ta²ₗ ta²ᵣ -> k (ta²ₗ :>- maₗ) (ta²ᵣ :> Just aᵣ)
          do maₗ <&> \aₗ -> (Top aₗ, ta² :>- Just aᵣ)
      _ -> Nothing
    (q, 0) -> case ta of
      Top _ -> Nothing
      ta² :>- maₗ -> 
        (<|>)
          do loop q ta² \ta²r 



    ((q, 1), (Nothing, _)) -> Nothing
    ((q, 0), _) 
    let (mta, f) 
          | r == 1  = pop ta
          | otherwise = (Just ta, id)
    in
    if q == 0
      then 





  case
    
    
    


  case n `quotRem` 2 of
    (0, q) -> 
    (Nothing, _) -> Nothing
    (Just taₗ, a) -> k taₗ (Top a)
  loop (



case  of
  n -> This ta
   -> That ta


  (q, 0, Top _) -> That original
  (q, _1, Top _) -> That original
  (q, 0, ta² :>- ma) -> _



  let (q,r) = n `quotRem` 2
  in case ta of
      Top _
        | n == 0    -> This ta
        | otherwise -> That ta
      ta² :>- ma  -> case (fiss q ta², r, ma) of
        (This _, 1, Just a) -> These (ta² :>- Nothing) (Top a)
        (This _, 0, _)      -> This ta

        (That _, 0, Just a) -> These (Top a) (ta² :>- Nothing)
        (That _, 0, _)      -> That ta
              This _ -> This ta
              That _ -> case ma of
                Nothing -> That ta
                Just a  -> These (Top a) (ta² :>- Nothing)
              These ta²₀ ta²₁ ->
                These (ta²₀ :>- ma) (ta²₁ :>- Nothing)
                Nothing -> These 

  where (q,r) = n `quotRem` 2
  
