module IntSpec where

import Prelude ()
import NanoPrelude

-- Type for an instance of Num
type NumInst a = (a->a->a, a->a->a, 
		  a->a->a, a->a->a, 
		  a->a->a, a->a->a) 

-- Projection functions for Num
numAdd :: NumInst a -> a -> a -> a
-- numAdd d = fst d
numAdd (_, _, _, _, _, op) = op

-- numMul :: NumInst a -> a -> a -> a
-- numMul d = snd d

-- A Num instance for primitive ints
intInst :: NumInst Int
intInst = ((+), (-), (*), (+), (-), (+))

-- A polymorphic sum over Nums
sumPoly inst acc xs = foldr (numAdd inst) acc xs

-- A specialised sum over primitive ints
sumInt = sumPoly intInst 0

-- Sum some numbers.
-- main = sumInt [1,2,3,4,5,6,7,8,9,10] + sumInt [11,12,13,14,15,16,17,18,19,20]
main = sumInt [1..50] + sumInt [51..100]

-- I would hope that:
--   1) The `inst` dictionary lookup is optimised away when `sumInt` calls sum, and
--   2) We also get the optimised version immediately when we do the second call to `sumInt`
--
-- If that's the case, the number of cycles should be pretty similar to the
-- "raw" primitive version in `main_raw`.

-- main = foldr (+) 0 [1,2,3,4,5,6,7,8,9,10] + foldr (+) 0 [11,12,13,14,15,16,17,18,19,20]

