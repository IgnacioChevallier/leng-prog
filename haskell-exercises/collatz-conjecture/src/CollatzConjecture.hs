module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = findCollatz n 0

findCollatz :: Integer -> Integer -> Maybe Integer
findCollatz n steps
  | n <= 0 = Nothing
  | n == 1 = Just steps
  | even n = findCollatz (n `div` 2) (steps + 1)
  | otherwise = findCollatz (3 * n + 1) (steps + 1)
