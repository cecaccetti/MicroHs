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

main :: TF
main = and T F
