module NanoPrelude(
  module Data.Int,
  module Data.Bool,
  module NanoPrelude
) where

import Prelude()
import Data.Int
import Data.Bool
  
infixl 6 +,-
infixl 7 *

(+) :: Int -> Int -> Int
(+) = primitive "+"

(-) :: Int -> Int -> Int
(-) = primitive "-"

(*) :: Int -> Int -> Int
(*) = primitive "*" 

infix 4 ==,/=
(==) :: Int -> Int -> Bool
(==) = primitive "=="

(/=) :: Int -> Int -> Bool
(/=) = primitive "/="

