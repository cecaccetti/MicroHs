module LitMatch(main) where
import Prelude

f :: Int -> Int
f 0 = 10
f 1 = 20
f (-1) = 30
f n = 0

g :: Int -> Int -> Int
g 1 0 = 10
g 2 0 = 20
g 2 1 = 21
g 1 1 = 11
g _ _ = 99

h :: Char -> Int
h 'a' = 1
h 'b' = 2
h _ = 3

s :: String -> Int
s "apa" = 1
s "foo" = 2
s _ = 3

main :: IO ()
main = do
  putStrLn $ show [f 0, f 1, f 10, f (-1)]
  putStrLn $ show [g 1 0, g 1 1, g 2 0, g 2 1, g 2 2]
  putStrLn $ show [h 'a', h 'b', h 'c']
  putStrLn $ show [s "aaa", s "apa", s "foo"]
