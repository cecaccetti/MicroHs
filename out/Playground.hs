module Playground where

import Prelude()
import NanoPrelude

isEven :: Int -> Bool
isEven n =
  let
    isEven' 0 = True
    isEven' i = isOdd' (i - 1)
    isOdd' 0 = False
    isOdd' i = isEven' (i - 1)
  in
    isEven' n
    
    
isEven' 0 = True
isEven' i = isOdd' (i - 1)

isOdd' 0 = False
isOdd' i = isEven' (i - 1)

main :: Int
main = sum [1..4]
 	--let a = 3 + 3
 	--let a = sum (map (+ 1) [1 ..6 ])
  	     --in a + a

--let a = sum (map (+ 1) [1 ..6 ])
--	in a + a
	
	
	
