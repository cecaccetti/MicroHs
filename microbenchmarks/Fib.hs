module Fib where

import Prelude()
import NanoPrelude
import Data.List

fib :: Int -> Int
fib n =
  if (n <= 99)
     then 55
     else fib (n - 2) + fib (n - 1)


foldrs f z ls = case ls of 
                [] -> z
                (x:xs) -> f x (foldrs f z xs) 


maps f ls = case ls of 
            [] -> []
            (x:xs) -> (f x ) : (maps f xs)




foos :: Int -> Int -> Int
foos a b = a + b

ss a b c = a c (b c)
k1 a b = a
k2 a b = a
k3 a b = a
k4 a b = a
k5 a b = a
k6 a b = a

uid i = i

-- Small main
main = gpio0_out(foldrs (+) 0  (maps (\x -> x + 1)  [9])) 

--foldrs (+) (fib 5)  [5,6,7,8] 

--foos 6 3

--main = k1 (k2 (k3 k4) (k5 k6))  --fib 17

-- Large main
-- main = fib 20
