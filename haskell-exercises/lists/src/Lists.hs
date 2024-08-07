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
intersection (x:xs) (y:ys)
  | null xs = []
  | member x (y:ys) = x : intersection xs (y:ys)

difference:: [Int] -> [Int] -> [Int]
difference (x:xs) (y:ys)
  | null xs = []
  | member x (y:ys) = difference xs (y:ys)
  | otherwise = x : difference xs (y:ys)

insert:: Int -> [Int] -> [Int]
insert e (x,xs)
  | x == [] = [e]
  | otherwise = e : x : xs

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
binaryToDecimal = error "Implement it"

toDecimal :: Int -> [Int] -> Int
toDecimal = error "Implement it"

toDec::Int -> String -> Int
toDec base s = error "Implement it"


-- Same as `toDec` But use a list comprehension


decimal::Int -> String -> Int
decimal  = error "Implement it"


firsts::[a] -> [[a]]
firsts = error "Implement it"


-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation


binaryAdd::String -> String -> String
binaryAdd  = error "Implement it"
