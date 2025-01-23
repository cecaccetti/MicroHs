module T where

import Prelude()
import NanoPrelude

type D a = (a->a->a, a->a->a)

dInt :: D Int
dInt = ((+),(*))

f :: D a -> a -> a -> a
f d x y =
  snd d y $  -- y *
  fst d x x  --     (x + x)

g :: Int -> Int -> Int
g = f dInt

h x y = y * (x + x)

main :: Int
main = 
  g 10 3 + g 1 2 + g 2 1 + g 3 3 -- 10 * (3+3) + 1 * (2+2) = 64

