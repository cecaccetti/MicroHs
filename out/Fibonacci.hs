module Fibonacci where

import Prelude()
import NanoPrelude

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib5 :: Int
fib5 = fib (2 + 1 + (1 + 1))

main :: Int
main = fib5
