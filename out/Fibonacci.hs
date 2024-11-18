module Fibonacci where

import Prelude()
import NanoPrelude

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib5 :: Int
fib5 = fib (2 + 1 + (1 + 1))

-- 0 1 1 2 3 5 8 13 21
main :: Int
main = fib 8
