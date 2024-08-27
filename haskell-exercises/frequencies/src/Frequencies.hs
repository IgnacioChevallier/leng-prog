module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies "" = []
frequencies str = insertionSort (getFrequencies str [])

getFrequencies :: String -> [Char] -> [Frequency]
getFrequencies [] _ = []
getFrequencies (x:xs) list =
    if notMember x list
    then (count x (xs), x) : getFrequencies xs (x : list)
    else getFrequencies xs list

count :: Eq a => a -> [a] -> Int
count x = (1 + ) . length . filter (== x)

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap [] = Map.empty
frequencyMap (x:xs) = frequencyMapBuilder x (x:xs) Map.empty

frequencyMapBuilder :: (Ord a) => a -> [a] -> Map a Int -> Map a Int
frequencyMapBuilder _ [] mapa = mapa
frequencyMapBuilder x (_:ys) mapa =
    if Map.notMember x mapa
    then frequencyMapBuilder (head ys) ys (Map.insert x (count x ys) mapa)
    else frequencyMapBuilder (head ys) ys mapa

notMember :: Eq a => a -> [a] -> Bool
notMember _ [] = True
notMember y (x:xs)
  | x == y    = False
  | otherwise = notMember y xs

insert::(Ord a) => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
  | e == x || e < x = e : x : xs
  | otherwise       = x : insert e xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort xs = let
    low = lowerFinder (head xs) xs
    remaining = removeItem low xs
  in low : insertionSort remaining

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem e (x:xs)
  | e == x    = xs
  | otherwise = x : removeItem e xs

lowerFinder :: Ord a => a -> [a] -> a
lowerFinder low [] = low
lowerFinder low (x:xs)
  | low > x   = lowerFinder x xs
  | otherwise = lowerFinder low xs
