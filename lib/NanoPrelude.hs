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

data Cmp = EQ | LT | GT

data Maybe a = Nothing | Just a

isNothing Nothing  = True
isNothing (Just x) = False

isJust Nothing  = False
isJust (Just x) = True

fromJust (Just x) = x
fromJust Nothing = primitive "error4"

maybe n j Nothing  = n
maybe n j (Just x) = j x

id :: forall a . a -> a
id i = i

flip f y x = f x y

const c x = c

infixr 0 $

($) :: forall a b . (a -> b) -> a -> b
f $ x = f x

curry f x y = f (x, y)

uncurry f (x,y) = f x y

infixr 9 .

(.) f g x = f (g x)

--(||) False x = x
--(||) True x  = True

--(&&) False x = False
--(&&) True x  = x

--not False = True
--not True  = False
  
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

infix 4 <,<=,>,>=
(<) :: Int -> Int -> Bool
(<) = primitive "<"
(<=) :: Int -> Int -> Bool
(<=) = primitive "<="
(>) :: Int -> Int -> Bool
(>) = primitive ">"
(>=) :: Int -> Int -> Bool
(>=) = primitive ">="

negate x = primitive "neg"

min x y = if x <= y then x else y

max x y = if x <= y then y else x

abs n = if 0 <= n then n else 0 - n

div x y = case divMod x y of (d, m) -> d

mod x y = case divMod x y of (d, m) -> m

divMod x y = let  y2 = y + y in
             if y2 <= x
               then case divMod x y2 of
                      (d2, m2) ->
                        let d2x2 = d2 + d2
                        in if y <= m2
                             then (d2x2 + 1, m2 - y)
                             else (d2x2, m2)
               else if y <= x
                      then (1::Int, x - y)
                      else (0::Int, x    )
                      
succ x = x + 1

pred x = x - 1

maximum :: [Int] -> Int
maximum [] = 0
maximum (x:ys) = foldr (\ y m -> if y > m then y else m) x ys

enumFrom n = n : enumFrom (n+1)

enumFromTo l h = takeWhile (<= h) (enumFrom l)

head (x : xs) = x
head [] = primitive "error3"

tail (x : xs) = xs
tail [] = []

init :: forall a . [a] -> [a]
init [] = primitive "error1" -- error "init: []"
init [_] = []
init (x:xs) = x : init xs

takeWhile :: forall a . (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) =
  if p x then
    x : takeWhile p xs
  else
    []

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

foldr' _ z [] = z
foldr' f z (a:as) = f a (foldr' f z as)
  
foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

concat :: forall a . [[a]] -> [a]
concat = foldr (++) []

length :: forall a . [a] -> Int
length =
  let
    rec :: forall a . Int -> [a] -> Int
    rec acc [] = acc
    rec acc (n:ns) = rec (acc+1) ns
  in rec 0

sum :: [Int] -> Int
sum = foldr (+) 0

null :: forall a . [a] -> Bool
null [] = True
null _ = False

foldr1 :: forall a . (a -> a -> a) -> [a] -> a
foldr1 f [] = primitive "error0"
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

all :: forall a . (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

any :: forall a . (a -> Bool) -> [a] -> Bool
any p []       = False
any p (x : xs) = (p x) || (any p xs)

elem x []       = False
elem x (y : ys) =
  if x == y
    then True
    else elem x ys

replicate :: forall a . Int -> a -> [a]
replicate n x 
    | n <= 0    = []
    | otherwise = x : replicate (n-1) x
    
repeat x = x : repeat x
    
filter :: forall a . (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
    
zip :: forall a b . [a] -> [b] -> [(a, b)]
zip [] ys = []
zip (x : xs) [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

unzip :: forall a b . [(a, b)] -> ([a], [b])
unzip []             = ([],[])
unzip ((x, y) : xys) =
  let  u = unzip xys
  in  (x : (fst u), y : (snd u))
  
and []       = True
and (b : bs) = if b then and bs else False

lookup a [] = Nothing
lookup a ((b, c) : rest) =
  if a == b
    then Just c
    else lookup a rest
  
fst :: forall a b . (a, b) -> a
fst (a, _) = a

snd :: forall a b . (a, b) -> b
snd (_, b) = b
