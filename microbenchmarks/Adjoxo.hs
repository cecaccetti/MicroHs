module Adjoxo where

import Prelude()
import NanoPrelude

data Outcome =
    Draw
  | Loss
  | Win

data Player =
    O
  | X

insert x [] = x : []
insert x (y : ys) =
  if x <= y
    then x : y : ys
    else y : insert x ys

bestOf Win  v = Win
bestOf Loss v = v
bestOf Draw Win = Win
bestOf Draw Draw = Draw
bestOf Draw Loss = Draw

inverse Loss = Win
inverse Draw = Draw
inverse Win  = Loss

cmp a b =
  if a == b
    then EQ
    else
      if a <= b
        then LT
        else GT

diff [] ys = []
diff (x : xs) [] = x : xs
diff (x : xs) (y : ys) =
  case cmp x y of
    LT -> x : diff xs (y : ys)
    EQ -> diff xs ys
    GT -> diff (x : xs) ys

subset xs ys = null (diff xs ys)

hasLine p =
  subset [1,2,3] p ||
  subset [4,5,6] p ||
  subset [7,8,9] p ||
  subset [1,4,7] p ||
  subset [2,5,8] p ||
  subset [3,6,9] p ||
  subset [1,5,9] p ||
  subset [3,5,7] p

gridFull ap pp = length ap + length pp == 9

analysis ap pp =
  if hasLine pp
    then Loss
    else
      if gridFull ap pp
        then Draw
        else foldr1 bestOf (map (moveval ap pp)
                                (diff (diff (enumFromTo 1 9) ap) pp))

moveval ap pp m = inverse (analysis pp (insert m ap))

adjudicate os xs =
  case (cmp (length os) (length xs)) of
    GT -> report (analysis xs os) X
    LT -> report (analysis os xs) O
    EQ -> if hasLine xs
            then report Win X
            else if hasLine os
                   then report Win O
                   else report (analysis xs os) X

report Loss s = side (opp s)
report Win  s = side s
report Draw p = 3::Int -- 'D'

opp O = X
opp X = O

side O = 0::Int -- 'O'
side X = 88::Int -- 'X'

-- Small main
main = adjudicate [1,4] [2,5]

-- Large main
-- main = adjudicate [] []
