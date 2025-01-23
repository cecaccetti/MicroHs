module FL where

import Prelude()
import NanoPrelude

fib :: Int -> Int
fib n =
  if n <= 1
     then 1
     else fib (n - 2) + fib (n - 1)

f :: Int -> Int -> Int
f x y = y + fib x

g :: Int -> Int
g = f 10

main = sum (map g [1..8])
