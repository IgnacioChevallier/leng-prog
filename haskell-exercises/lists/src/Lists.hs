module Lists (member, union, intersection, difference,
              insert, insertionSort, firsts,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd) where

import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys
  | member x ys = x : intersection xs ys
  | otherwise = intersection xs ys


difference:: [Int] -> [Int] -> [Int]
difference [] _ = []
difference xs [] = xs
difference (x:xs) (y:ys)
  | member x (y:ys) = difference xs (y:ys)
  | otherwise = x : difference xs (y:ys)

insert:: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs)
  | e == x || e < x = e : x : xs
  | otherwise       = x : insert e xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort xs = let
    low = lowerFinder (head xs) xs
    remaining = removeItem low xs
  in low : insertionSort remaining

removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem e (x:xs)
  | e == x    = xs
  | otherwise = x : removeItem e xs

lowerFinder :: Int -> [Int] -> Int
lowerFinder low [] = low
lowerFinder low (x:xs)
  | low > x   = lowerFinder x xs
  | otherwise = lowerFinder low xs

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = x * 2 ^ length xs + binaryToDecimal xs

toDecimal :: Int -> [Int] -> Int
toDecimal _ [] = 0
toDecimal 0 _ = 0
toDecimal base (x:xs) = x * base ^ length xs + toDecimal base xs

toDec::Int -> String -> Int
toDec _ "" = 0
toDec 0 _ = 0
toDec base str = toDecimal base (map digitToInt str)


-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal _ [] = 0
decimal base str = (x * base ^ length xs + decimal base (eraseFirstChar str))
    where x:xs = map digitToInt str

eraseFirstChar :: String -> String
eraseFirstChar [] = []
eraseFirstChar (_:xs) = xs

firsts :: [a] -> [[a]]
firsts [] = []
firsts (x:xs) = doFirsts (x:xs) (length (x:xs)) 1

doFirsts :: [a] -> Int -> Int -> [[a]]
doFirsts lst n m
    | m > n = []
    | otherwise = take m lst : doFirsts lst n (m + 1)

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd :: String -> String -> String
binaryAdd a b = toBinary(show (binaryToDecimal (map digitToInt a) + binaryToDecimal (map digitToInt b)))

toBinary :: String -> String
toBinary "0" = "0"
toBinary "1" = "1"
toBinary n = toBinary (show (div (read n) 2)) ++ show (mod (read n) 2)