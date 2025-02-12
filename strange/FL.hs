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

f' :: Int -> (Int -> Int) -> Int -> Int
f' x y z = z + (y x + y 10)

g' :: Int -> Int
g' = f' 8 fib

fRec :: Int -> Int -> Int -> Int
fRec x y z = 
  let 
    fib' :: Int -> Int
    fib' n = 
      if n <= 1 then 1 else fib' (n-2) + fib' (n-1)
  in z + (fib' y + fib' x)
  
gRec :: Int -> Int
gRec = fRec 10 8

main = sum (map g' [1..20])
