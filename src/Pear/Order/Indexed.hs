module Pear.Order.Indexed where

import Data.Kind (Type)
import Pear.Positive.Kind

type Order :: Type -> Type -> Type
data Order i j

range :: KnownPositive n => Order (Fin n) (Fin n)
range = undefined

(·) :: Order j k -> Order i j -> Order i k
(·) = undefined

reindex :: Order i j -> i -> j
reindex = undefined

invert :: Order i j -> Order j i
invert = undefined

ofuse :: Order i (Fin n) -> Order j (Fin m) -> Order (Either i j) (Fin (Plus n m))
ofuse = undefined

ofiss :: Order (Fin n) i -> Order (Fin m) j -> Order (Fin (Plus n m)) (Either i j)
ofiss = undefined

oappend :: Order i (Fin n) -> Order j (Fin m) -> Order (Either i j) (Fin (Plus n m))
oappend = undefined

osplit :: Order (Fin n) i -> Order (Fin m) j -> Order (Fin (Plus n m)) (Either i j)
osplit = undefined

oreverse :: Order i j -> Order i j
oreverse = undefined
