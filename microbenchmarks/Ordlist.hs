module Ordlist where

import Prelude()
import NanoPrelude

data Nat
  = S Nat
  | Z

implies False x = True
implies True  x = x

ord []           = True
ord [x]          = True
ord (x : y : ys) = (implies x y) && (ord (y : ys))

ins x [] = [x]
ins x (y : ys) =
  if implies x y
    then x : y : ys
    else y : ins x ys

prop x xs = implies (ord xs) (ord (ins x xs))

boolList Z = [[]]
boolList (S n) =
  boolList n ++
  map ((:) False) (boolList n) ++
  map ((:) True ) (boolList n)

top n = and $
        map (prop True ) (boolList n) ++
        map (prop False) (boolList n)

-- Small main
main =
  let num = S $ S $ S $ S Z
  in if top num
       then 1::Int
       else 0

{-
-- Large main
main =
  let num = S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S Z
  in if top num
       then 1
       else 0
-}
