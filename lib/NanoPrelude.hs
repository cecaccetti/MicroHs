module NanoPrelude(
  module Data.Int,
  module Data.Bool,
  module Data.List_Type,
  module NanoPrelude
) where

import Prelude()
import Data.Int
import Data.Bool
import Data.List_Type
  
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

map :: forall a b . (a -> b) -> [a] -> [b]
map f =
  let
    rec [] = []
    rec (a : as) = f a : rec as
  in rec
  
foldr :: forall a b . (a -> b -> b) -> b -> [a] -> b
foldr f z =
  let
    rec [] = z
    rec (x : xs) = f x (rec xs)
  in rec
  
foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

sum :: [Int] -> Int
sum = foldr (+) 0
  
