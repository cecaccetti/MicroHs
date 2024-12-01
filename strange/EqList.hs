module EqList where

import Prelude()
import NanoPrelude

eqList :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f [] [] = True
eqList f [] (_ : _) = False
eqList f (_ : _) [] = False
eqList f (x : xs) (y : ys) = f x y && eqList f xs ys

eqList' :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList' f la lb =
  if notnull la && notnull lb then f (head la) (head lb) && eqList' f (tail la) (tail lb)
  else if null la && null lb then True
  else False
  
eqList'' :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList'' f (x : xs) (y : ys) = f x y && eqList'' f xs ys
eqList'' _ la lb = eqListEmpty la lb

eqListEmpty [] [] = True
eqListEmpty _ _ = False
  
notnull [] = False
notnull (_:_) = True

boolToInt :: Bool -> Int
boolToInt True = 42
boolToInt False = 0

lessThan :: Int -> Int -> Bool
lessThan a b = a < b

main :: Int
main = boolToInt (eqList lessThan (enumFromTo 1 100) (enumFromTo 101 200))
	
