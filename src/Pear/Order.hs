module Pear.Order where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Pear.Positive

type Order :: Type -> Type -> Type
data Order i j

range :: Positive -> Order Natural Natural
range = undefined

-- a hypo-category
(·) :: Order j k -> Order i j -> Order i k
(·) = undefined

reindex :: Order i j -> i -> j
reindex = undefined

-- reindex (a · invert a) == id
invert :: Order i j -> Order j i
invert = undefined

ofuse :: Order i n -> Order j n -> Order (Either i j) n
ofuse = undefined

ofiss :: Order n i -> Order n j -> Order n (Either i j)
ofiss = undefined

oappend :: Order i n -> Order j n -> Order (Either i j) n
oappend = undefined

osplit :: Order n i -> Order n j -> Order n (Either i j)
osplit = undefined

oreverse :: Order i j -> Order i j
oreverse = undefined
