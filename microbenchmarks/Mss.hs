module Mss where

import Prelude()
import NanoPrelude

inits :: [Int] -> [[Int]]
inits xs =
  case xs of
    []       -> [[]]
    (y : ys) -> xs : inits (init xs)

tails :: [Int] -> [[Int]]
tails [] = []
tails (x : xs) = (x : xs) : tails xs

segments :: [Int] -> [[Int]]
segments xs = concatMap tails (inits xs)

mss :: [Int] -> Int
mss xs = maximum (map sum (segments xs))
 
-- Small main
main = mss [(0-20)..20] -- mss [1..3]-- sum (takeWhile (<= 3) [2,2,2,3,2,19,32])--maximum [1..2] -- foldl (-) 0 [1..3]--  -- (enumFromTo (0 - 20) 20)

{-
-- Large main
main = (mss (enumFromTo (0 - 160) 160) -
        mss (enumFromTo (0 - 150) 150)   ) +
       (mss (enumFromTo (0 - 161) 161) -
        mss (enumFromTo (0 - 151) 151)   )
-}
