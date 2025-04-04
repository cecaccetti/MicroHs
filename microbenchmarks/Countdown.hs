module Countdown where

import Prelude()
import NanoPrelude

data Op a =
    Add
  | Div
  | Mul
  | Sub
  | Val a
  | App (Op a) (Op a) (Op a)

valid Add x y  =  True
valid Sub x y  =  not (x <= y)
valid Mul x y  =  True
valid Div x y  =  mod x y == 0
valid _ _ _ = False

apply Add x y  =  x + y
apply Sub x y  =  x - y
apply Mul x y  =  mul x y
apply Div x y  =  div x y
apply _ _ _ = 0

subs []       = [[]]
subs (x : xs) = let yss = subs xs
    in yss ++ map ((:) x) yss

interleave x []       =  [[x]]
interleave x (y : ys) =
  (x : y : ys) : map ((:) y) (interleave x ys)

perms []       =  [[]]
perms (x : xs) =  concatMap (interleave x) (perms xs)

choices xs  =  concatMap perms (subs xs)

split [] = []
split (x : xs) =
  if null xs
    then []
    else ([x], xs) : map (cross (((:) x), id)) (split xs)

results []       =  []
results (n : ns) =
  if null ns
    then [(Val n, n)]
    else concatMap combinedResults (split (n : ns))

combinedResults (ls, rs)  = concatProdWith combine (results ls) (results rs)

concatProdWith f []       ys = []
concatProdWith f (x : xs) ys = concatMap (f x) ys ++ concatProdWith f xs ys

combine (l, x) (r, y) =
  let ops = [Add, Sub, Mul, Div]
  in concatMap (combi l x r y) ops

combi l x r y o =
  if valid o x y
    then [(App o l r, apply o x y)]
    else []

solutions ns n = concatMap (solns n) (choices ns)

solns n ns = let ems = results ns
             in preImage n (results ns)

preImage n []             = []
preImage n ((e, m) : ems) =
  if m == n
    then e : preImage n ems
    else preImage n ems

mul x n =
  if n == 1
    then x
    else case divMod n 2 of
           (d, m) -> mul (x + x) d + (if m == 0 then 0 else x)

cross (f, g) (x, y) = (f x, g y)

-- Small main
main =
  let givens = [3::Int,4,10]
      target = 70::Int
  in length (solutions givens target)

{-
-- Large main
main =
  let givens = [1,3,7,10,25]
      target = 765
  in length (solutions givens target)
-}
