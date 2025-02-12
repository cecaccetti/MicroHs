module PolySum where

import Prelude ()
import Primitives
import Data.Int
import Data.Bool
import Data.List_Type
import NanoPrelude(enumFromTo)

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  -- negate :: a -> a
  
instance Num Int where
  (+)  = primitive "+"
  (-)  = primitive "-"
  (*)  = primitive "*"
  -- negate = primitive "neg"

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = if x == y then False else True

instance Eq Int where
  (==) = primitive "=="
  (/=) = primitive "/="

class Eq a => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

instance Ord Int where
  (<) = primitive "<"
  (<=) = primitive "<="
  (>) = primitive ">"
  (>=) = primitive ">="
  
sum :: Num a => a -> [a] -> a
sum z ns = 
  let
    go [] acc = acc
    go (n:ns) acc = go ns (n+acc)
  in go ns z

maximum :: Ord a => a -> [a] -> a
maximum z ns =
  let
    go [] acc = acc
    go (n:ns) acc = go ns (max acc n)
  in go ns z

isum :: [Int] -> Int
isum = sum 0

imaximum :: [Int] -> Int
imaximum = maximum 0

replicate :: Int -> a -> [a]
replicate n x =
  if n == 0 then []
  else x : replicate (n-1) x

map :: forall a b . (a -> b) -> [a] -> [b]
map f =
  let
    rec [] = []
    rec (a : as) = f a : rec as
  in rec
  
main :: Int
main = isum (map imaximum (replicate 3 [1,2]))

