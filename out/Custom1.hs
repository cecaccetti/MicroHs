module Custom1 where

import Prelude()
import NanoPrelude

data TF = T | F

and :: TF -> TF -> TF
and T T = T
and _ _ = F

or :: TF -> TF -> TF
or F F = F
or _ _ = T

neg :: TF -> TF
neg T = F
neg F = T

data Pat = X | At Pat Pat

holes :: Pat -> Int
holes X = 1
holes (At p1 p2) = holes p1 + holes p2

main :: Int
main = holes (At (At (At X X) X) (At X X))
