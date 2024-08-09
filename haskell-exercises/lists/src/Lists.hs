module Lists (member, union, intersection, difference,
              insert, insertionSort,
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
difference [] xs = xs
difference xs [] = xs
difference (x:xs) (y:ys)
  | null xs = []
  | member x (y:ys) = difference xs (y:ys)
  | otherwise = x : difference xs (y:ys)

insert:: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs) = e : x : xs

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
toDec base str


-- Same as `toDec` But use a list comprehension


decimal::Int -> String -> Int
decimal  = error "Implement it"


firsts::[a] -> [[a]]
firsts = error "Implement it"


-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation


binaryAdd::String -> String -> String
binaryAdd  = error "Implement it"
